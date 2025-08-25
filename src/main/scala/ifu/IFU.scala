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
		val ifu2ICache	= Decoupled(new IFU2ICache)
		val icache2IFU 	= Flipped(Decoupled(new ICache2IFU))
		val flush 		= Input(Bool())
		val correctPC 	= Input(UInt(32.W))
		val fromPC 		= Input(UInt(32.W))
    })
	/* Module */
	val branchPredict 	= Module(new BranchPredict(8, 2, 10, 1, ReplacePolicy.LRU, DPPolicy.BTFN))

	/* HandShake */

	/* BPU */
	branchPredict.io.correctPC	:= io.correctPC
	branchPredict.io.fromPC 	:= io.fromPC
	branchPredict.io.flush 		:= io.flush
	branchPredict.io.ifu2ICache	<> io.ifu2ICache

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val ifuGetInstCounter = RegInit(0.U(32.W))
		val IGIC 			= Module(new PerformanceCounter)
		when(io.inst.ready) {
			when(io.inst.valid) {
				ifuGetInstCounter := 1.U
			} .otherwise {
				ifuGetInstCounter := ifuGetInstCounter + 1.U
			}
		} .otherwise {
			ifuGetInstCounter := 0.U
		}
		IGIC.io.valid		:= io.inst.ready & io.inst.valid
		IGIC.io.counterType	:= PerformanceCounterType.IFUGETINST.asUInt
		IGIC.io.data 		:= ifuGetInstCounter
	}

	io.icache2IFU.ready		:= io.inst.ready
	io.inst.bits.pc			:= io.icache2IFU.bits.pc
	io.inst.bits.inst		:= io.icache2IFU.bits.inst
	io.inst.valid			:= io.icache2IFU.valid
}

class BranchPredict(depthOfTable: Int, offsetWidth: Int, tagWidth: Int, way: Int, raPolicy: ReplacePolicy.Type, dpPolicy: DPPolicy.Type) extends Module {
	val io = IO(new Bundle {
		val correctPC 	= Input(UInt(32.W))
		val fromPC 		= Input(UInt(32.W))
		val flush 		= Input(Bool())
		val ifu2ICache	= Decoupled(new IFU2ICache)	
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
	} .elsewhen(io.ifu2ICache.ready) {
		pcReg 			:= Mux(hitWire, Mux(jumpWire, btbPCWire, normalPC), normalPC)
	}

	io.ifu2ICache.valid 	:= !io.flush
	io.ifu2ICache.bits.pc	:= pcReg
}

class directionPredictor(dpPolicy: DPPolicy.Type) extends Module {
	val io = IO(new Bundle {
		val pc 		= Input(UInt(32.W))
		val nextPC	= Input(UInt(32.W))
		val jump 	= Output(Bool())
	})

	io.jump := (io.nextPC < io.pc)
}
