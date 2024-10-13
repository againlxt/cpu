package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
//import singlecyclecpu._

class IFU extends Module {
    val io = IO(new Bundle {
		val pc  	 = Input(UInt(32.W))
        val memData  = Input(UInt(32.W))
        val inst     = Decoupled(new IFU2IDU)
    })

	val pcReg 		= RegInit(0.U(32.W))
	val memDataReg 	= RegInit(0.U(32.W))
	pcReg 			:= io.pc
    memDataReg 		:= io.memData
	when(pcReg =/= io.pc) {
		io.inst.valid := 1.B
	} .otherwise {
		io.inst.valid := 0.B
	}

    io.inst.bits.inst  := memDataReg
    io.inst.bits.pc    := pcReg
}
