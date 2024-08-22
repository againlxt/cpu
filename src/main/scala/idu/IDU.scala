package idu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq

import idu._

class IDU extends Module {
    val io = IO(new Bundle{
		val cmd 	 = Input(UInt(32.W))

		// Input from Riscv32BaseReg
		val rs1Data = Input(UInt(32.W))
		val rs2Data = Input(UInt(32.W))

		val immType = Output(UInt(3.W))
		val regWR 	= Output(UInt(1.W))
		val srcAALU = Output(UInt(1.W))
		val srcBALU = Output(UInt(2.W))
		val ctrALU 	= Output(UInt(4.W))
		val branch 	= Output(UInt(3.W))
		val memToReg= Output(UInt(1.W))
		val memWR 	= Output(UInt(1.W))
		val memOP 	= Output(UInt(3.W))

		// Output to Riscv32BaseReg
		val rs1Index 	= Output(UInt(5.W))
		val rs2Index 	= Output(UInt(5.W))
		val rdIndex  	= Output(UInt(5.W))
		
		// Output to EXU
		val rs1 	 	= Output(UInt(32.W))
		val rs2 	 	= Output(UInt(32.W))
		val imm 	 	= Output(UInt(32.W))
	})

    val func7Wire  	= io.cmd(31, 25)
    val rs2IndexWire    	= io.cmd(24, 20)
    val rs1IndexWire    	= io.cmd(19, 15)
    val func3Wire  	= io.cmd(14, 12)
    val rdIndexWire     	= io.cmd(11, 7)
    val opcodeWire 	= io.cmd(6, 0)
    val iImmWire   	= io.cmd(31, 20)
    val sImmWire   	= Cat(io.cmd(31, 25), io.cmd(11, 7))
    val bImmWire   	= Cat(io.cmd(31), io.cmd(7), io.cmd(30, 25), io.cmd(11, 8), 0.U(1.W))
    val uImmWire   	= Cat(io.cmd(31, 12), 0.U(12.W))
    val jImmWire   	= Cat(io.cmd(31), io.cmd(19, 12), io.cmd(20), io.cmd(30, 21), 0.U(1.W))

	// Instantitate ContrGen
    val contrGen 	= Module(new ContrGen())
	// Input
	contrGen.io.cmd 	:= io.cmd;
    contrGen.io.opcode  := opcodeWire
    contrGen.io.func3 	:= func3Wire
    contrGen.io.func7 	:= func7Wire
	// Output
    val immTypewire	= contrGen.io.immType
    val regWRWire 	= contrGen.io.regWR
    val srcAALUWire = contrGen.io.srcAALU
    val srcBALUWire = contrGen.io.srcBALU
    val ctrALUWire 	= contrGen.io.ctrALU
    val branchWire 	= contrGen.io.branch
    val memToRegWire= contrGen.io.memToReg
    val memWRWire 	= contrGen.io.memWR
    val memOPWire 	= contrGen.io.memOP

	// Instantitate ImmGen
    val immGen 		= Module(new ImmGen)
	// Input
    immGen.io.iImm 	:= iImmWire
    immGen.io.sImm 	:= sImmWire
    immGen.io.bImm 	:= bImmWire
    immGen.io.uImm 	:= uImmWire
    immGen.io.jImm 	:= jImmWire
    immGen.io.immType 	:= immTypewire
	// Output
    val immWire 	= immGen.io.imm	

	// Output
    io.immType 		:= immTypewire
    io.regWR 	 	:= regWRWire
    io.srcAALU 		:= srcAALUWire
    io.srcBALU 		:= srcBALUWire
    io.ctrALU 		:= ctrALUWire
    io.branch 		:= branchWire
    io.memToReg 	:= memToRegWire
    io.memWR 		:= memWRWire
    io.memOP 		:= memOPWire

    io.rs1Index 	:= rs1IndexWire
    io.rs2Index 	:= rs2IndexWire
    io.rdIndex 		:= rdIndexWire

    io.rs1 			:= io.rs1Data
    io.rs2 			:= io.rs2Data
    io.imm 			:= immWire
}
