package interface

import chisel3._
import chisel3.util._
import chisel3.util.Decoupled

/* Bus */
class IFU2IDU extends Bundle {
    val inst = Output(UInt(32.W))
	val pc 	 = Output(UInt(32.W))
}

class IDU2EXU extends Bundle {
	val pc 		= Output(UInt(32.W))
	val rs1Data = Output(UInt(32.W))
	val rs2Data = Output(UInt(32.W))
	val imm 	= Output(UInt(32.W))
	val inst 	= Output(UInt(32.W))

	val regWR 	= Output(UInt(1.W))
	val srcAALU = Output(UInt(2.W))
	val srcBALU = Output(UInt(2.W))
	val ctrALU 	= Output(UInt(4.W))
	val branch 	= Output(UInt(4.W))
	val toReg 	= Output(UInt(2.W))
	val memWR 	= Output(UInt(1.W))
	val memValid= Output(UInt(1.W))
	val memOP 	= Output(UInt(3.W))
	val rs1Index= Output(UInt(5.W))
	val ecall 	= Output(UInt(1.W))
	val mret 	= Output(UInt(1.W))
	val csrEn 	= Output(UInt(1.W))
	val csrWr 	= Output(UInt(1.W))
	val csrOP 	= Output(UInt(1.W))
	val csrALUOP= Output(UInt(2.W))
}

class EXU2WBU extends Bundle {
	val pc 			= Output(UInt(32.W))
	val memData		= Output(UInt(32.W))
	val aluData		= Output(UInt(32.W))
	val csrWData	= Output(UInt(32.W))
	val csrData 	= Output(UInt(32.W))
	val immData 	= Output(UInt(32.W))
	val rs1Data 	= Output(UInt(32.W))
	val inst 		= Output(UInt(32.W))

	val regWR 		= Output(UInt(1.W))
	val memWR 		= Output(UInt(1.W))
	val memValid	= Output(UInt(1.W))
	val memOP 		= Output(UInt(3.W))
	val toReg 		= Output(UInt(2.W))
	val branchCtr 	= Output(UInt(4.W))
	val less 		= Output(UInt(1.W))
	val zero 		= Output(UInt(1.W))
	val ecall 		= Output(UInt(1.W))
	val csrEn 		= Output(UInt(1.W))
	val csrWr 		= Output(UInt(1.W))
}

/* Normal */
class C2IFU extends Bundle {
    val memData = Input(UInt(32.W))
}

class IDU2BaseReg extends Bundle {
	val rs1Data = Input(UInt(32.W))
	val rs2Data = Input(UInt(32.W))

	val rs1Index = Output(UInt(4.W))
	val rs2Index = Output(UInt(4.W))
}

class EXU2CSR extends Bundle {
	val csrData	= Input(UInt(32.W))
	val mret 	= Output(UInt(1.W))
	val ecall 	= Output(UInt(1.W))
	val csr 	= Output(UInt(12.W))
}

class WBU2CSR extends Bundle {
	val pc 		= Output(UInt(32.W))
	val csrWData= Output(UInt(32.W))
	val csr 	= Output(UInt(12.W))
	val ecall 	= Output(UInt(1.W))
	val csrEn 	= Output(UInt(1.W))
	val csrWr 	= Output(UInt(1.W))
}

class WBU2BaseReg extends Bundle {
	val rdIndex = Output(UInt(4.W))
	val data 	= Output(UInt(32.W))
	val regWR 	= Output(UInt(1.W))
}

class WBU2PC extends Bundle {
	val nextPC	= Output(UInt(32.W))
}

class IFUSRAM extends Bundle {
	val ren 	= Input(UInt(1.W))
	val addr 	= Input(UInt(32.W))
	val valid 	= Output(UInt(1.W))
	val data 	= Output(UInt(32.W))
}

class WBUSRAM extends Bundle {
	val clk 	= Output(UInt(1.W))
	val raddr	= Output(UInt(32.W))
	val ren		= Output(UInt(1.W))
	val rdata	= Input(UInt(32.W))
	val waddr	= Output(UInt(32.W))
	val wdata 	= Output(UInt(32.W))
	val wen 	= Output(UInt(1.W))
	val wmask 	= Output(UInt(4.W))
}
