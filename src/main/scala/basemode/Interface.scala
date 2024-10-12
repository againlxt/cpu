package interface

import chisel3._
import chisel3.util._
import chisel3.util.Decoupled

/* Bus */
class IFU2IDU extends Bundle {
    val inst = Output(UInt(32.W))
}

class IDU2EXU extends Bundle {
	val rs1Data = Output(UInt(32.W))
	val rs2Data = Output(UInt(32.W))
	val imm 	= Output(UInt(32.W))

	val regWR 	= Output(UInt(1.W))
	val srcAALU = Output(UInt(2.W))
	val srcBALU = Output(UInt(2.W))
	val ctrALU 	= Output(UInt(4.W))
	val branch 	= Output(UInt(4.W))
	val toReg 	= Output(UInt(2.W))
	val memWR 	= Output(UInt(1.W))
	val memValid= Output(UInt(1.W))
	val memOP 	= Output(UInt(3.W))
	val ecall 	= Output(UInt(1.W))
	val mret 	= Output(UInt(1.W))
	val csrEn 	= Output(UInt(1.W))
	val csrWr 	= Output(UInt(1.W))
	val csrOP 	= Output(UInt(1.W))
	val csrALUOP= Output(UInt(2.W))
}

class IDU2BaseReg extends Bundle {
	val rs1Data = Input(UInt(32.W))
	val rs2Data = Input(UInt(32.W))

	val rs1Index 	= Output(UInt(5.W))
	val rs2Index 	= Output(UInt(5.W))
	val rdIndex  	= Output(UInt(5.W))
}

/* Normal */
class C2IFU extends Bundle {
    val memData = Input(UInt(32.W))
}
