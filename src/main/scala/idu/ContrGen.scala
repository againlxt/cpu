package idu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq
import common._

class ContrGen extends Module {
	val io = IO(new Bundle{
		val cmd  	= Input(UInt(32.W))
		val opcode 	= Input(UInt(7.W))
		val func3 	= Input(UInt(3.W))
		val func7 	= Input(UInt(7.W))

		val immType = Output(UInt(3.W))
		val regWR 	= Output(UInt(1.W))
		val srcAALU = Output(UInt(1.W))
		val srcBALU = Output(UInt(2.W))
		val ctrALU 	= Output(UInt(4.W))
		val branch 	= Output(UInt(3.W))
		val memToReg= Output(UInt(1.W))
		val memWR 	= Output(UInt(1.W))
		val memValid= Output(UInt(1.W))
		val memOP 	= Output(UInt(3.W))
	})

	val opcodeWire 	= io.opcode
	val func3Wire 	= io.func3
	val func7Wire 	= io.func7

	val instructionFormatWire = MuxCase(InstructionFormat.NOP, Seq(
		(io.cmd(19,0) === "b00001000000001100111".U).asBool -> InstructionFormat.RET,

		(func7Wire === "b0000000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.ADD,
		(func3Wire === "b000".U && opcodeWire === "b0010011".U).asBool -> InstructionFormat.ADDI,
		(opcodeWire === "b0110111".U).asBool -> InstructionFormat.LUI,
		(opcodeWire === "b0010111".U).asBool -> InstructionFormat.AUIPC,
		(func3Wire === "b010".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.SLTI,
		(opcodeWire === "b1101111".U).asBool -> InstructionFormat.JAL,
		(func3Wire === "b000".U & opcodeWire === "b1100111".U).asBool -> InstructionFormat.JALR,
		(func3Wire === "b010".U & opcodeWire === "b0100011".U).asBool -> InstructionFormat.SW,
		(func3Wire === "b000".U & opcodeWire === "b1100011".U).asBool -> InstructionFormat.BEQ
	))

	val instructionTypeWire = MuxCase(InstructionType.NOP, Seq(
		(func7Wire === "b0000000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func3Wire === "b000".U && opcodeWire === "b0010011".U) -> InstructionType.I,
		(opcodeWire === "b0110111".U) -> InstructionType.U,
		(opcodeWire === "b0010111".U) -> InstructionType.U,
		(func3Wire === "b010".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(opcodeWire === "b1101111".U).asBool -> InstructionType.J,
		(func3Wire === "b000".U & opcodeWire === "b1100111".U).asBool -> InstructionType.I,
		(func3Wire === "b010".U & opcodeWire === "b0100011".U).asBool -> InstructionType.S,
		(func3Wire === "b000".U & opcodeWire === "b1100011".U).asBool -> InstructionType.B
	))

	io.regWR    := 0.U
	io.srcAALU  := 0.U
	io.srcBALU  := 0.U
	io.ctrALU   := 0.U
	io.branch   := 0.U
	io.memToReg := 0.U
	io.memWR    := 0.U
	io.memValid := 0.U
	io.memOP    := 0.U

	io.immType 	:= instructionTypeWire.asUInt
	switch(instructionFormatWire) {
		is(InstructionFormat.RET) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 1.U
			io.srcBALU 	:= 2.U
			io.ctrALU 	:= 2.U
			io.branch 	:= 2.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.ADD) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= 0.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.ADDI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= 0.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.LUI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 1.U
			io.ctrALU 	:= 3.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.AUIPC) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 1.U
			io.srcBALU 	:= 1.U
			io.ctrALU 	:= 0.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.JAL) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 1.U
			io.srcBALU 	:= 2.U
			io.ctrALU 	:= 0.U
			io.branch 	:= 1.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.JALR) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 1.U
			io.srcBALU 	:= 2.U
			io.ctrALU 	:= 2.U
			io.branch 	:= 2.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.BEQ) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= 2.U
			io.branch 	:= 4.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.SW) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 1.U
			io.ctrALU 	:= 0.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 1.U
			io.memValid := 1.U
			io.memOP 	:= 2.U
		}
		is(InstructionFormat.NOP) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= 0.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
	}

	val cgDPIC = Module(new CGDPIC())
	cgDPIC.io.cmd 					:= io.cmd
	cgDPIC.io.instructionFormat 	:= instructionFormatWire.asUInt
	cgDPIC.io.instructionFormatJAL 	:= InstructionFormat.JAL.asUInt
	cgDPIC.io.instructionFormatJALR := InstructionFormat.JALR.asUInt
	cgDPIC.io.instructionFormatRET 	:= InstructionFormat.RET.asUInt
}

class CGDPIC extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle{
		val cmd 					= Input(UInt(32.W))
		val instructionFormat 		= Input(UInt(32.W))
		val instructionFormatJAL	= Input(UInt(32.W))
		val instructionFormatJALR	= Input(UInt(32.W))
		val instructionFormatRET	= Input(UInt(32.W))
	})

	setInline("CGDPIC.sv",
	"""module CGDPIC(
	|	input [31:0] cmd,
	|	input [31:0] instructionFormat,
	|	input [31:0] instructionFormatJAL,
	|	input [31:0] instructionFormatJALR,
	|	input [31:0] instructionFormatRET
	|);
	|import "DPI-C" function void sim_exit();
	|always @(cmd) begin
	|    if(cmd==32'h00100073)   sim_exit();
	|end
	|
	|import "DPI-C" function void set_ftrace_function_call_flag();
	|always @(instructionFormat) begin
	|	if((instructionFormat==instructionFormatJAL || instructionFormat==instructionFormatJALR)) 
	|		set_ftrace_function_call_flag();
	|end
	|
	|import "DPI-C" function void set_ftrace_ret_flag();
	|always @(instructionFormat) begin
	|	if(instructionFormat==instructionFormatRET) set_ftrace_ret_flag();
	|end
	|
	|export "DPI-C" function getCommond;
	|function bit [31:0] getCommond;
	|	return cmd;
	|endfunction
	|endmodule
	""".stripMargin)
}
