package exu

import chisel3._
import chisel3.util._
import memory._
import exu._
import alu._
import common._
import singlecyclecpu._
import memory.ReadWriteSmem

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

		val dataOut = Output(UInt(32.W))
	})
	val addrWire 	= io.addr
	val memOPWire 	= io.memOP
	val dataInWire 	= io.dataIn
	val wrEnWire 	= io.wrEn

	val lenWire 	= MuxCase (1.U(3.W), Seq(
		(memOPWire === "b000".U).asBool -> 1.U(3.W),
		(memOPWire === "b001".U).asBool -> 2.U(3.W),
		(memOPWire === "b010".U).asBool -> 4.U(3.W),
		(memOPWire === "b101".U).asBool -> 2.U(3.W),
		(memOPWire === "b100".U).asBool -> 1.U(3.W)
	))
	val sOrUWire 	= MuxCase (0.U(1.W), Seq(
		(memOPWire === "b000".U).asBool -> 1.U(1.W),
		(memOPWire === "b001".U).asBool -> 1.U(1.W),
		(memOPWire === "b010".U).asBool -> 1.U(1.W),
		(memOPWire === "b101".U).asBool -> 0.U(1.W),
		(memOPWire === "b100".U).asBool -> 0.U(1.W)
	))
	val dataMem 	= Module(new ReadWriteSmem(32, 32, Integer.parseInt("79999999", 16)))
	dataMem.io.enable 	:= wrEnWire
	dataMem.io.write 	:= wrEnWire
	dataMem.io.addr 	:= addrWire
	dataMem.io.len 		:= lenWire
	dataMem.io.dataIn 	:= dataInWire
	val signdataWire 	= Wire(SInt(32.W))
	val unsigndataWire  = Wire(UInt(32.W))
	signdataWire 		:= dataMem.io.dataOut.asSInt
	unsigndataWire		:= dataMem.io.dataOut.asUInt

	io.dataOut 			:= Mux(sOrUWire.asBool, signdataWire.asUInt, unsigndataWire)
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
