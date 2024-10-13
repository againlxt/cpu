package singlecyclecpu

import chisel3._
import chisel3.util._
import singlecyclecpu._
import ifu._
import idu._
import exu._
import alu._
import common._
import wbu.WBU

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
	if(io_memData[19:0]==20'b00001_000_00000_11001_11) set_ftrace_ret_flag();
end

export "DPI-C" function getCommond;
function bit [31:0] getCommond;
	return _ifu_io_cmd;
endfunction
*/

object Main extends App {
	emitVerilog(new top, Array("--split-verilog" ,"--target-dir", "generated"))
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
	val csrReg 			= Module(new CSRReg)
	val idu 			= Module(new IDU)
	val exu 			= Module(new EXU)
	val wbu 			= Module(new WBU)
	val memDataWire 	= io.memData

	// PC Reg
	pc.io.npcState 	:= io.npcState
	pc.io.wbu2PC 	:= wbu.io.wbu2PC
	val pcWire 		= pc.io.pc
	io.curPC     	:= pcWire

	// IFU
	// Input
	ifu.io.pc 		:= pcWire
	ifu.io.memData 	:= memDataWire
	// Output
	ifu.io.inst <> idu.io.inst

	// IDU
	idu.io.idu2EXU 		<> exu.io.idu2EXU
	idu.io.idu2BaseReg	<> riscv32BaseReg.io.idu2BaseReg

	// EXU
	exu.io.exu2WBU	<> wbu.io.exu2WBU
	exu.io.exu2CSR 	<> csrReg.io.exu2CSR

	// WBU
	wbu.io.wbu2CSR		<> csrReg.io.wbu2CSR
	wbu.io.wbu2BaseReg	<> riscv32BaseReg.io.wbu2BaseReg
}
