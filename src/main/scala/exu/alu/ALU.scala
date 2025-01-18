package alu

import chisel3._
import chisel3.util._
import baseexu._
import alu._

class ALU extends Module {
	val io = IO(new Bundle {
		val aluCtr 		= Input(UInt(4.W))
		val srcAData 	= Input(UInt(32.W))
		val srcBData 	= Input(UInt(32.W))
		
		val less 		= Output(UInt(1.W))
		val zero 		= Output(UInt(1.W))
		val aluOut 		= Output(UInt(32.W))
	})

	val aluControl 			= Module(new ALUControl)
	aluControl.io.aluCtr 	:= io.aluCtr
	val aluCtrWire 			= io.aluCtr(2, 0)
	val aOrLCtrWire 		= aluControl.io.aOrLCtr
	val lOrRCtrWire 		= aluControl.io.lOrRCtr
	val uOrSCtrWire 		= aluControl.io.uOrSCtr
	val subOrAddCtrWire 	= aluControl.io.subOrAddCtr

	val shifter				= Module(new Shifter)	
	shifter.io.lOrR 		:= lOrRCtrWire
	shifter.io.aOrL 		:= aOrLCtrWire
	shifter.io.dIn 			:= io.srcAData
	shifter.io.shamt 		:= io.srcBData(4,0)
	val shiftDataWire 		= shifter.io.dOut

	val aluAdder 			= Module(new ALUAdder)
	aluAdder.io.subOrAdd 	:= subOrAddCtrWire
	aluAdder.io.srcAData 	:= io.srcAData
	aluAdder.io.srcBData 	:= io.srcBData
	val carryWire 			= aluAdder.io.carry
	val zeroWire 			= aluAdder.io.zero
	val overflowWire 		= aluAdder.io.overflow
	val resultWire 			= aluAdder.io.result

	val lessWire 			= Mux(uOrSCtrWire.asBool, carryWire^subOrAddCtrWire, 
								overflowWire^resultWire(31))

	val adderDataWire 		= resultWire
	val sltDataWire 		= Cat(Fill(31, 0.U), lessWire)
	val bDataWire 			= io.srcBData
	val xorDataWire 		= io.srcAData ^ io.srcBData
	val orDataWire 			= io.srcAData | io.srcBData
	val andDataWire 		= io.srcAData & io.srcBData
	val aluOutWire  		= MuxCase( 0.U(32.W), Seq(
		(aluCtrWire === ALUOutType.ADDER.asUInt).asBool 	-> adderDataWire,
		(aluCtrWire === ALUOutType.LSHIFT.asUInt).asBool	-> shiftDataWire,
		(aluCtrWire === ALUOutType.SLT.asUInt).asBool 	-> sltDataWire,
		(aluCtrWire === ALUOutType.B.asUInt).asBool 		-> bDataWire,
		(aluCtrWire === ALUOutType.XOR.asUInt).asBool 	-> xorDataWire,
		(aluCtrWire === ALUOutType.RSHIFT.asUInt).asBool -> shiftDataWire,
		(aluCtrWire === ALUOutType.OR.asUInt).asBool 	-> orDataWire,
		(aluCtrWire === ALUOutType.AND.asUInt).asBool 	-> andDataWire,
	))

	io.less 	:= lessWire
	io.zero 	:= zeroWire
	io.aluOut 	:= aluOutWire
}

object ALUOutType extends ChiselEnum {
	val ADDER, LSHIFT, SLT, B, XOR, RSHIFT, OR, AND, NOP=Value
}

class ALUAdder extends Module {
	val io = IO(new Bundle {
		val subOrAdd 	= Input(UInt(1.W))
		val srcAData 	= Input(UInt(32.W))
		val srcBData 	= Input(UInt(32.W))

		val carry 		= Output(UInt(1.W))
		val zero		= Output(UInt(1.W))
		val overflow 	= Output(UInt(1.W))
		val result 		= Output(UInt(32.W))
	})

	val cla32Add 	= Module(new CLAGen(32))
	val subOrAddWire= Cat(Fill(31, io.subOrAdd), io.subOrAdd)
	val aAddWire 	= io.srcAData
	val bAddWire 	= io.srcBData
	val bXorCIn 	= bAddWire ^ subOrAddWire

	cla32Add.io.a 	:= aAddWire
	cla32Add.io.b 	:= bAddWire ^ subOrAddWire
	cla32Add.io.cin := io.subOrAdd
	io.carry 		:= cla32Add.io.cout
	io.zero			:= Mux(cla32Add.io.sum === 0.U, 1.U, 0.U)
	val aOverflow	= (aAddWire(31) === bAddWire(31)) && (cla32Add.io.sum(31) =/= aAddWire(31))
	val sOverflow  	= (aAddWire(31) === bXorCIn(31)) && (cla32Add.io.sum(31) =/= aAddWire(31))
	io.overflow		:= Mux(io.subOrAdd.asBool, sOverflow, aOverflow)
	io.result 		:= cla32Add.io.sum
}

class Shifter extends  Module {
	val io = IO(new Bundle {
		val lOrR 	= Input(UInt(1.W))
		val aOrL 	= Input(UInt(1.W))
		val dIn 	= Input(UInt(32.W))
		val shamt	= Input(UInt(5.W))

		val dOut	= Output(UInt(32.W))
	})
	val dataS 					= Cat(Fill(32, io.dIn(31)), io.dIn.asUInt)
	val rightShiftedDataWire	= dataS >> io.shamt
	val leftShiftedDataWire 	= io.dIn << io.shamt
	io.dOut := Mux(io.lOrR.asBool, leftShiftedDataWire, Mux(io.aOrL.asBool, rightShiftedDataWire(31, 0), io.dIn >> io.shamt))
}
