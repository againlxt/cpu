package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
import memory._
import javax.management.modelmbean.ModelMBean
import basemode.Delay
import basemode.AXIAccessFault
import cpu.Config
import dpic._
import dpic.PerformanceCounterType.{IFUGETINST => IFUGETINST}
object ReplacePolicy extends ChiselEnum {
	val LRU, FIFO, RANDOM = Value
}
object DPPolicy extends ChiselEnum {
	val BTFN = Value
}
class IFU extends Module {
    val io = IO(new Bundle {
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Icache	= new IFU2Icache
		val flush 		= Input(Bool())
		val correctPC 	= Input(UInt(32.W))
		val fromPC 		= Input(UInt(32.W))
    })
	val s_idle :: s_wait_bp :: s_wait_icache :: s_wait :: Nil = Enum(4)
	val state 	= RegInit(s_idle)
	state 	:= MuxLookup(state, s_idle)(List(
		s_idle 			-> s_wait_icache,
		s_wait_bp 		-> Mux(io.flush, s_wait_bp, s_wait_icache),
		s_wait_icache	-> Mux(io.flush, s_wait, 
		Mux(io.inst.valid & io.inst.ready, s_wait_bp, s_wait_icache)),
		s_wait			-> Mux(io.ifu2Icache.oEnable, s_wait_bp, s_wait)
	))
	
	val handWire = io.inst.valid & io.inst.ready
	val branchPredict = Module(new BranchPredict(8, 2, 10, 1, ReplacePolicy.LRU, DPPolicy.BTFN))
	branchPredict.io.correctPC	:= io.correctPC
	branchPredict.io.fromPC 	:= io.fromPC
	branchPredict.io.flush 		:= io.flush
	branchPredict.io.next 		:= handWire
	val pc 	= branchPredict.io.predictPC

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val ifuGetInstCounter = RegInit(0.U(32.W))
		when (state === s_wait_bp) {
			ifuGetInstCounter := 0.U
		} .otherwise {
			ifuGetInstCounter := ifuGetInstCounter + 1.U
		}
		val IGIC 			= Module(new PerformanceCounter)
		IGIC.io.valid		:= io.ifu2Icache.oEnable
		IGIC.io.counterType	:= PerformanceCounterType.IFUGETINST.asUInt
		IGIC.io.data 		:= ifuGetInstCounter
	}

	io.ifu2Icache.enable:= ((state === s_idle) | (state === s_wait_bp)) & (!io.flush)
	io.ifu2Icache.addr	:= pc
	io.inst.valid		:= io.ifu2Icache.oEnable & (state === s_wait_icache) & (!io.flush)
	io.inst.bits.inst	:= io.ifu2Icache.inst
	io.inst.bits.pc		:= pc
}

class BranchPredict(depthOfTable: Int, offsetWidth: Int, tagWidth: Int, way: Int, raPolicy: ReplacePolicy.Type, dpPolicy: DPPolicy.Type) extends Module {
	val io = IO(new Bundle {
		val correctPC 	= Input(UInt(32.W))
		val fromPC 		= Input(UInt(32.W))
		val flush 		= Input(Bool())
		val next 		= Input(Bool())
		val predictPC 	= Output(UInt(32.W))
	})
	/* BTB */
	val pcReg 			= RegInit(Mux(Config.SoC.asBool, "h30000000".U(32.W), "h80000000".U(32.W)))
	val indexWidth 		= log2Up(depthOfTable/way)
	val indexWire 		= pcReg(indexWidth+offsetWidth-1, offsetWidth)
	val tagWire 		= pcReg(tagWidth+indexWidth+offsetWidth-1, indexWidth+offsetWidth)
	object BTB extends ChiselEnum {
		val validVec 	= RegInit(VecInit(Seq.fill(depthOfTable)(VecInit(Seq.fill(way)(false.B)))))
		val tagVec 		= RegInit(VecInit(Seq.fill(depthOfTable)(VecInit(Seq.fill(way)(0.U((tagWidth).W))))))
		val pcVec 		= RegInit(VecInit(Seq.fill(depthOfTable)(VecInit(Seq.fill(way)(0.U(32.W))))))
	}
	val wayValidVec 	= BTB.validVec(indexWire)
	val wayTagVec		= BTB.tagVec(indexWire)
	val hitVec 			= wayValidVec.zip(wayTagVec).map { case (v, t) => v && (t === tagWire) }
	val hitWire     	= hitVec.reduce(_ || _)
	val hitWay 			= PriorityEncoder(hitVec)
	val btbPCWire 		= BTB.pcVec(indexWire)(hitWay)
	val fromPCReg		= RegEnable(io.fromPC, io.flush) 	

	/* RA */
	val flushReg 		= RegNext(io.flush)
	val ra 				= Module(new Replacement_Algorithm(way, depthOfTable, indexWidth, raPolicy))
	ra.io.update_entry	:= flushReg
	ra.io.update_index	:= indexWire
	val wayIndexWire 	= ra.io.way_index
	val flushIndexWire 	= io.fromPC(indexWidth+offsetWidth-1, offsetWidth)
	when(flushReg) {
		BTB.validVec(flushIndexWire)(wayIndexWire) 	:= 1.B
		BTB.tagVec(flushIndexWire)(wayIndexWire)	:= fromPCReg(tagWidth+indexWidth+offsetWidth-1, indexWidth+offsetWidth)
		BTB.pcVec(flushIndexWire)(wayIndexWire)		:= pcReg
	}

	/* DP */
	val dp 			= Module(new directionPredictor(dpPolicy))
	dp.io.pc		:= pcReg
	dp.io.nextPC	:= btbPCWire
	val jumpWire 	= dp.io.jump
	val normalPC 	= pcReg + 4.U
	when(io.flush) {
		pcReg 			:= io.correctPC
	} .elsewhen(io.next) {
		pcReg 			:= Mux(hitWire, Mux(jumpWire, btbPCWire, normalPC), normalPC)
	}

	io.predictPC	:= pcReg
}

class directionPredictor(dpPolicy: DPPolicy.Type) extends Module {
	val io = IO(new Bundle {
		val pc 		= Input(UInt(32.W))
		val nextPC	= Input(UInt(32.W))
		val jump 	= Output(Bool())
	})

	io.jump := (io.nextPC < io.pc)
}
