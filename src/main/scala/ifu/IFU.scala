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
	val s_idle :: s_wait_bp :: s_wait_icache :: Nil = Enum(3)
	val state 	= RegInit(s_idle)
	state 	:= MuxLookup(state, s_idle)(List(
		s_idle 			-> s_wait_icache,
		s_wait_bp 		-> Mux(io.flush, s_wait_bp, s_wait_icache),
		s_wait_icache	-> Mux(io.ifu2Icache.oEnable | io.flush, 
		s_wait_bp , s_wait_icache)
	))

	val branchPredict = Module(new BranchPredict)
	branchPredict.io.correctPC	:= io.correctPC
	branchPredict.io.flush 		:= io.flush
	branchPredict.io.next 		:= io.ifu2Icache.oEnable 
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
	io.inst.valid		:= io.ifu2Icache.oEnable
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
	pcReg 			:= Mux(io.next, Mux(io.flush, io.correctPC, pcReg+4.U), pcReg)

	io.predictPC	:= pcReg
}
