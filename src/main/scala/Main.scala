package singlecyclecpu

import chisel3._
import chisel3.util._
import singlecyclecpu._
import ifu._
import idu._
import exu._
import common._

/* 
import "DPI-C" function void sim_exit();
always @(io_memData) begin
    if(io_memData==32'h00100073)   sim_exit();
end

import "DPI-C" function void set_ftrace_function_call_flag();
always @(io_memData) begin
	if(io_memData[11:0]==12'b00001_11011_11) set_ftrace_function_call_flag();
end

import "DPI-C" function void set_ftrace_ret_flag();
always @(io_memData) begin
	if(io_memData[16:0]==17'b00001_000_00000_11001_11) set_ftrace_ret_flag();
end

export "DPI-C" function getCommond;
function bit [31:0] getCommond;
	return _ifu_io_cmd;
endfunction
*/

object Main extends App {
	emitVerilog(new top, Array("--target-dir", "generated"))
}

class top extends Module {
	val io = IO(new Bundle {
		val npcState 	= Input(UInt(3.W))
		val memData 	= Input(UInt(32.W))
		val curPC 		= Output(UInt(32.W))
		val nextPC 		= Output(UInt(32.W))
	})
	val pc 				= Module(new PC)
	val ifu 			= Module(new IFU)
	val riscv32BaseReg 	= Module(new Riscv32BaseReg)
	val idu 			= Module(new IDU)
	val exu 			= Module(new EXU)
	val memDataWire 	= io.memData

	// PC Reg
	pc.io.npcState 	:= io.npcState
	pc.io.dnpc 		:= io.nextPC
	val pcWire 		= pc.io.pc
	io.curPC     	:= pcWire

	// IFU
	// Input
	ifu.io.pc 		:= pcWire
	ifu.io.memData 	:= memDataWire
	// Output
	val cmdWire 	= ifu.io.cmd

	// IDU
	// Input
	idu.io.cmd 		:= cmdWire
	val rs1DataWire = Wire(UInt(32.W))
	val rs2DataWire = Wire(UInt(32.W))
	// Output
	val immTypeWire = idu.io.immType
	val regWRWire 	= idu.io.regWR
	val srcAALUWire = idu.io.srcAALU
	val srcBALUWire = idu.io.srcBALU
	val ctrALUWire 	= idu.io.ctrALU
	val branchWire 	= idu.io.branch
	val memToRegWire= idu.io.memToReg
	val memWRWire 	= idu.io.memWR
	val memOPWire 	= idu.io.memOP
	val rs1IndexWire= idu.io.rs1Index
	val rs2IndexWire= idu.io.rs2Index
	val rdIndexWire = idu.io.rdIndex
	val rs1Wire 	= idu.io.rs1
	val rs2Wire 	= idu.io.rs2
	val immWire 	= idu.io.imm

	// Base Reg
	// Input
	riscv32BaseReg.io.rs1Index 	:= rs1IndexWire
	riscv32BaseReg.io.rs2Index 	:= rs2IndexWire
	riscv32BaseReg.io.rdIndex 	:= rdIndexWire
	val dataInWire 	= Wire(UInt(32.W))
	riscv32BaseReg.io.regWR 	:= regWRWire
	// Output
	rs1DataWire 				:= riscv32BaseReg.io.rs1Data
	rs2DataWire 				:= riscv32BaseReg.io.rs2Data

	// EXU
	// Input
	exu.io.npcState 		:= io.npcState
	exu.io.rs1Data 			:= rs1DataWire
	exu.io.rs2Data 			:= rs2DataWire
	exu.io.immData 			:= immWire
	exu.io.pc 				:= pcWire
	exu.io.aluASrcCtr 		:= srcAALUWire
	exu.io.aluBSrcCtr 		:= srcBALUWire
	exu.io.aluCtr 			:= ctrALUWire
	exu.io.memOPCtr 		:= memOPWire
	exu.io.memWRCtr 		:= memWRWire
	exu.io.branchCtr 		:= branchWire
	exu.io.memToRegCtr		:= memToRegWire
	// Output
	io.nextPC 				:= exu.io.nextPC
	dataInWire 				:= exu.io.rdData

	idu.io.rs1Data 	:= rs1DataWire
	idu.io.rs2Data 	:= rs2DataWire
	riscv32BaseReg.io.dataIn 	:= dataInWire
}
