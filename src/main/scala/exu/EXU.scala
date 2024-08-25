package exu

import chisel3._
import chisel3.util._
import memory._
import exu._
import alu._
import common._
import singlecyclecpu._
import memory.ReadWriteSmem
import chisel3.util.HasBlackBoxResource
import java.time.Clock

class EXU extends Module {
	val io = IO(new Bundle {
		// Input
		val npcState    = Input(UInt(3.W))
		val rs1Data 	= Input(UInt(32.W))
		val rs2Data 	= Input(UInt(32.W))
		val immData 	= Input(UInt(32.W))
		val pc 			= Input(UInt(32.W))

		// Contral Data
		val aluASrcCtr 	= Input(UInt(1.W))
		val aluBSrcCtr 	= Input(UInt(2.W))
		val aluCtr 		= Input(UInt(4.W))
		val memOPCtr 	= Input(UInt(3.W))
		val memWRCtr	= Input(UInt(1.W))
		val memValidCtr	= Input(UInt(1.W))
		val branchCtr 	= Input(UInt(3.W))
		val memToRegCtr = Input(UInt(1.W))

		// Output
		val nextPC 		= Output(UInt(32.W))
		val rdData 		= Output(UInt(32.W))
	})

	// Wire
	val rs1DataWire 	= io.rs1Data
	val rs2DataWire 	= io.rs2Data
	val immDataWire 	= io.immData
	val pcWire			= io.pc
	val aluASrcCtrWire 	= io.aluASrcCtr
	val aluBSrcCtrWire 	= io.aluBSrcCtr
	val aluCtrWire 		= io.aluCtr
	val memOPCtrWire 	= io.memOPCtr
	val memWRCtrWire	= io.memWRCtr
	val memValidCtrWire = io.memValidCtr
	val branchCtrWire 	= io.branchCtr
	val memToRegCtrWire = io.memToRegCtr

	// ALU
	val srcADataWire 	= Mux(aluASrcCtrWire.asBool, pcWire, rs1DataWire)
	val srcBDataWire 	= MuxCase(0.U(32.W), Seq(
		(aluBSrcCtrWire === 0.U).asBool -> rs2DataWire,
		(aluBSrcCtrWire === 1.U).asBool -> immDataWire,
		(aluBSrcCtrWire === 2.U).asBool -> 4.U
	))
	val alu = Module(new ALU)
	// Input
	alu.io.aluCtr 		:= aluCtrWire
	alu.io.srcAData 	:= srcADataWire
	alu.io.srcBData 	:= srcBDataWire
	// Output
	val lessWire 	= alu.io.less
	val zeroWire 	= alu.io.zero
	val resultWire 	= alu.io.aluOut

	// Branch Cond
	val branchCond 		= Module(new BranchCond)
	// Input
	branchCond.io.branch 	:= branchCtrWire
	branchCond.io.less 	:= lessWire
	branchCond.io.zero 	:= zeroWire
	// Output
	val pcASrcWire 		= branchCond.io.pcASrc
	val pcBSrcWire 		= branchCond.io.pcBSrc

	// Data Memory
	val dataMem 		= Module(new DataMem)
	// Input
	dataMem.io.addr 		:= resultWire
	dataMem.io.memOP 		:= memOPCtrWire
	dataMem.io.dataIn 		:= rs2DataWire
	dataMem.io.wrEn 		:= memWRCtrWire
	dataMem.io.valid 		:= memValidCtrWire
	// Output
	val dataOutWire 	= dataMem.io.dataOut

	// Output
	io.nextPC 	:= Mux(pcASrcWire.asBool, immDataWire, 4.U) +
	Mux(pcBSrcWire.asBool, rs1DataWire, pcWire)
	io.rdData 	:= Mux(memToRegCtrWire.asBool, dataOutWire, resultWire)
}

class DataMem extends Module {
	val io = IO(new Bundle{
		// Input
		val addr 	= Input(UInt(32.W))
		val memOP 	= Input(UInt(3.W))
		val dataIn 	= Input(UInt(32.W))
		val wrEn 	= Input(Bool())
		val valid  	= Input(Bool())

		val dataOut = Output(UInt(32.W))
	})
	val addrWire 	= io.addr
	val memOPWire 	= io.memOP
	val dataInWire 	= io.dataIn
	val wrEnWire 	= io.wrEn
	val validWire 	= io.valid

	val wMaskWire 	= MuxCase (1.U(8.W), Seq(
		(memOPWire === "b000".U).asBool -> "b00000001".U,
		(memOPWire === "b001".U).asBool -> "b00000011".U,
		(memOPWire === "b010".U).asBool -> "b00001111".U,
		(memOPWire === "b101".U).asBool -> "b00000011".U,
		(memOPWire === "b100".U).asBool -> "b00000001".U
	))
	val sOrUWire 	= MuxCase (0.U(1.W), Seq(
		(memOPWire === "b000".U).asBool -> 1.U(1.W),
		(memOPWire === "b001".U).asBool -> 1.U(1.W),
		(memOPWire === "b010".U).asBool -> 1.U(1.W),
		(memOPWire === "b101".U).asBool -> 0.U(1.W),
		(memOPWire === "b100".U).asBool -> 0.U(1.W)
	))
	
	val dataMem 		= Module(new DataMemV())
	dataMem.io.clk 		:= this.clock.asUInt
	dataMem.io.addr 	:= addrWire
	dataMem.io.wmask 	:= wMaskWire
	dataMem.io.dataIn 	:= dataInWire
	dataMem.io.wrEn 	:= wrEnWire
	dataMem.io.valid 	:= validWire
	val signdataWire 	= Wire(SInt(32.W))
	val unsigndataWire  = Wire(UInt(32.W))
	signdataWire 		:= dataMem.io.dataOut.asSInt
	unsigndataWire		:= dataMem.io.dataOut

	io.dataOut 			:= Mux(sOrUWire.asBool, signdataWire.asUInt, unsigndataWire)
}

class DataMemV extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle{
		// Input
		val clk 	= Input(UInt(1.W))
		val addr 	= Input(UInt(32.W))
		val wmask 	= Input(UInt(8.W))
		val dataIn 	= Input(UInt(32.W))
		val wrEn 	= Input(Bool())
		val valid  	= Input(Bool())

		val dataOut = Output(UInt(32.W))
	})

	setInline("DataMemV.sv",
	"""module DataMemV(
	   |	input 		clk,
	   |	input [31:0] addr,
	   |	input [7:0]  wmask,
	   |	input [31:0] dataIn,
	   |	input 		 wrEn,
	   |	input 		 valid,
	   |
	   |	output [31:0] dataOut
	   |);
	   |reg[31:0] rdata;
	   |import "DPI-C" function int unsigned pmem_read(input int unsigned raddr);
	   |import "DPI-C" function void pmem_write(
	   |	input int unsigned waddr, input int unsigned wdata, input byte wmask);	
	   |always @(posedge clk) begin
	   |	if(wrEn & valid) begin
	   |		pmem_write(addr, dataIn, wmask);
	   |	end
	   |end
	   |assign dataOut = rdata;
	   |always @(*) begin
	   |	if(valid) begin
	   |		rdata = pmem_read(addr);
	   |	end
	   |	else begin
	   |		rdata = 32'd0;
	   |	end
	   |end
	   |endmodule
	""".stripMargin)
}

class BranchCond extends Module {
	val io = IO(new Bundle {
		// Input
		val branch 	= Input(UInt(3.W))
		val less 	= Input(Bool())
		val zero 	= Input(Bool())

		// Output
		val pcASrc 	= Output(Bool())
		val pcBSrc 	= Output(Bool())
	})

	val branchWire 	= io.branch
	val lessWire 	= io.less
	val zeroWire 	= io.zero

	io.pcASrc 	:= MuxCase (0.B, Seq(
		(branchWire === "b000".U).asBool -> 0.B,
		(branchWire === "b001".U).asBool -> 1.B,
		(branchWire === "b010".U).asBool -> 1.B,
		(branchWire === "b100".U & !zeroWire).asBool -> 0.B,
		(branchWire === "b100".U & zeroWire).asBool -> 1.B,
		(branchWire === "b101".U & !zeroWire).asBool -> 1.B,
		(branchWire === "b101".U & zeroWire).asBool -> 0.B,
		(branchWire === "b110".U & !lessWire).asBool -> 0.B,
		(branchWire === "b110".U & lessWire).asBool -> 1.B,
		(branchWire === "b111".U & !lessWire).asBool -> 1.B,
		(branchWire === "b111".U & lessWire).asBool -> 0.B
	))

	io.pcBSrc 	:= MuxCase (0.B, Seq(
		(branchWire === "b000".U).asBool -> 0.B,
		(branchWire === "b001".U).asBool -> 0.B,
		(branchWire === "b010".U).asBool -> 1.B,
		(branchWire === "b100".U & !zeroWire).asBool -> 0.B,
		(branchWire === "b100".U & zeroWire).asBool -> 0.B,
		(branchWire === "b101".U & !zeroWire).asBool -> 0.B,
		(branchWire === "b101".U & zeroWire).asBool -> 0.B,
		(branchWire === "b110".U & !lessWire).asBool -> 0.B,
		(branchWire === "b110".U & lessWire).asBool -> 0.B,
		(branchWire === "b111".U & !lessWire).asBool -> 0.B,
		(branchWire === "b111".U & lessWire).asBool -> 0.B
	))
}
