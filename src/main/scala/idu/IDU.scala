package idu

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import scala.collection.immutable.ArraySeq
import interface._

import idu._
import _root_.interface._

class IDU extends Module {
    val io = IO(new Bundle{
		val inst 			= Flipped(Decoupled(new IFU2IDU))
		val idu2IDU			= Decoupled(new IDU2EXU)
		val idu2BaseReg 	= Decoupled(new IDU2BaseReg)
	})

    val func7Wire  	= io.inst.bits.inst(31, 25)
    val rs2IndexWire= io.inst.bits.inst(24, 20)
    val rs1IndexWire= io.inst.bits.inst(19, 15)
    val func3Wire  	= io.inst.bits.inst(14, 12)
    val rdIndexWire = io.inst.bits.inst(11, 7)
    val opcodeWire 	= io.inst.bits.inst(6, 0)
    val iImmWire   	= io.inst.bits.inst(31, 20)
    val sImmWire   	= Cat(io.inst.bits.inst(31, 25), io.inst.bits.inst(11, 7))
    val bImmWire   	= Cat(io.inst.bits.inst(31), io.inst.bits.inst(7), io.inst.bits.inst(30, 25), io.inst.bits.inst(11, 8), 0.U(1.W))
    val uImmWire   	= Cat(io.inst.bits.inst(31, 12), 0.U(12.W))
    val jImmWire   	= Cat(io.inst.bits.inst(31), io.inst.bits.inst(19, 12), io.inst.bits.inst(20), io.inst.bits.inst(30, 21), 0.U(1.W))

	// Instantitate ContrGen
    val contrGen 	= Module(new ContrGen())
	// Input
    contrGen.io.cmd 	:= io.inst.bits.inst
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
    val memValidWire= contrGen.io.memValid
    val memOPWire 	= contrGen.io.memOP
    val ecallWire   = contrGen.io.ecall
    val mretWire    = contrGen.io.mret
    val csrEnWire   = contrGen.io.csrEn
    val csrWrWire   = contrGen.io.csrWr
    val csrOPWire   = contrGen.io.csrOP
    val csrALUOPWire= contrGen.io.csrALUOP

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

	// 

	// Output
    io.idu2IDU.bits.regWR 	 	:= regWRWire
    io.idu2IDU.bits.srcAALU 	:= srcAALUWire
    io.idu2IDU.bits.srcBALU 	:= srcBALUWire
    io.idu2IDU.bits.ctrALU 		:= ctrALUWire
    io.idu2IDU.bits.branch 		:= branchWire
    io.idu2IDU.bits.toReg 		:= memToRegWire
    io.idu2IDU.bits.memWR 		:= memWRWire
	io.idu2IDU.bits.memValid 	:= memValidWire 
    io.idu2IDU.bits.memOP 		:= memOPWire
    io.idu2IDU.bits.ecall       := ecallWire
    io.idu2IDU.bits.mret        := mretWire
    io.idu2IDU.bits.csrEn       := csrEnWire
    io.idu2IDU.bits.csrWr       := csrWrWire
    io.idu2IDU.bits.csrOP       := csrOPWire
    io.idu2IDU.bits.csrALUOP    := csrALUOPWire

    io.idu2BaseReg.bits.rs1Index := rs1IndexWire
    io.idu2BaseReg.bits.rs2Index := rs2IndexWire
    io.idu2BaseReg.bits.rdIndex := rdIndexWire

    io.idu2IDU.bits.rs1Data 	:= io.idu2BaseReg.bits.rs1Data
    io.idu2IDU.bits.rs2Data 	:= io.idu2BaseReg.bits.rs2Data
    io.idu2IDU.bits.imm 		:= immWire
}
