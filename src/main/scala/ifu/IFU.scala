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
class IFU extends Module {
    val io = IO(new Bundle {
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Icache	= new IFU2Icache
		val flush 		= Input(Bool())
		val correctPC 	= Input(UInt(32.W))
    })
	val s_idle :: s_wait_bp :: s_wait_icache :: s_wait :: Nil = Enum(4)
	val state 	= RegInit(s_idle)
	state 	:= MuxLookup(state, s_idle)(List(
		s_idle 			-> s_wait_icache,
		s_wait_bp 		-> s_wait_icache,
		s_wait_icache	-> Mux(io.flush, s_wait, 
		Mux(io.inst.valid & io.inst.ready, s_wait_bp, s_wait_icache)),
		s_wait			-> Mux(io.ifu2Icache.oEnable, s_wait_bp, s_wait)
	))
	
	val handWire = io.inst.valid & io.inst.ready
	val branchPredict = Module(new BranchPredict)
	branchPredict.io.correctPC	:= io.correctPC
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

	io.ifu2Icache.enable:= (state === s_idle) | (state === s_wait_bp)
	io.ifu2Icache.addr	:= pc
	io.inst.valid		:= io.ifu2Icache.oEnable & (state === s_wait_icache)
	io.inst.bits.inst	:= io.ifu2Icache.inst
	io.inst.bits.pc		:= pc
}

class BranchPredict extends Module {
	val io = IO(new Bundle {
		val correctPC 	= Input(UInt(32.W))
		val flush 		= Input(Bool())
		val next 		= Input(Bool())
		val predictPC 	= Output(UInt(32.W))
	})
	val pcReg 		= RegInit(Mux(Config.SoC.asBool, "h30000000".U(32.W), "h80000000".U(32.W)))
	when(io.flush) {
		pcReg 			:= io.correctPC
	} .elsewhen(io.next) {
		pcReg 			:= pcReg+4.U
	}

	io.predictPC	:= pcReg
}
