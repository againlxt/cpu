package idu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq
import common._
import common.InstructionType

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

	val cmdWire 	= io.cmd
	val opcodeWire 	= io.opcode
	val func3Wire 	= io.func3
	val func7Wire 	= io.func7

	val instructionFormatWire = MuxCase(InstructionFormat.NOP, Seq(
		// Pseudo-Instructions
		(io.cmd(19,0) === "b00001000000001100111".U).asBool -> InstructionFormat.RET,
		(cmdWire(31,15) === 0.U && cmdWire(11,7) === 0.U && func3Wire === "b000".U && opcodeWire === "b0010011".U).asBool -> InstructionFormat.NOP,

		// Normal-Instructions

		/* Integer Computational Instructions */
		// Integer Register-Immediate Instructions
		(func3Wire === "b000".U && opcodeWire === "b0010011".U).asBool -> InstructionFormat.ADDI,
		(func3Wire === "b010".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.SLTI,
		(func3Wire === "b011".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.SLTIU,
		(func3Wire === "b111".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.ANDI,
		(func3Wire === "b110".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.ORI,
		(func3Wire === "b100".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.XORI,
		(cmdWire(31,26) === "b000000".U & func3Wire === "b001".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.SLLI,
		(cmdWire(31,26) === "b000000".U & func3Wire === "b101".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.SRLI,
		(cmdWire(31,26) === "b010000".U & func3Wire === "b101".U & opcodeWire === "b0010011".U).asBool -> InstructionFormat.SRAI,
		(opcodeWire === "b0110111".U).asBool -> InstructionFormat.LUI,
		(opcodeWire === "b0010111".U).asBool -> InstructionFormat.AUIPC,
		// Integer Register-Register Operations
		(func7Wire === "b0000000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.ADD,
		(func7Wire === "b0000000".U && func3Wire === "b010".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.SLT,
		(func7Wire === "b0000000".U && func3Wire === "b011".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.SLTU,
		(func7Wire === "b0000000".U && func3Wire === "b111".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.AND,
		(func7Wire === "b0000000".U && func3Wire === "b110".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.OR,
		(func7Wire === "b0000000".U && func3Wire === "b100".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.XOR,
		(func7Wire === "b0000000".U && func3Wire === "b001".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.SLL,
		(func7Wire === "b0000000".U && func3Wire === "b101".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.SRL,
		(func7Wire === "b0100000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.SUB,
		(func7Wire === "b0100000".U && func3Wire === "b101".U && opcodeWire === "b0110011".U).asBool -> InstructionFormat.SRA,

		/* Control Transfer Instructions */
		// Unconditional Jumps
		(opcodeWire === "b1101111".U).asBool -> InstructionFormat.JAL,
		(func3Wire === "b000".U & opcodeWire === "b1100111".U).asBool -> InstructionFormat.JALR,
		// Conditional Branches
		(func3Wire === "b000".U & opcodeWire === "b1100011".U).asBool -> InstructionFormat.BEQ,
		(func3Wire === "b001".U & opcodeWire === "b1100011".U).asBool -> InstructionFormat.BNE,
		(func3Wire === "b100".U & opcodeWire === "b1100011".U).asBool -> InstructionFormat.BLT,
		(func3Wire === "b110".U & opcodeWire === "b1100011".U).asBool -> InstructionFormat.BLTU,
		(func3Wire === "b101".U & opcodeWire === "b1100011".U).asBool -> InstructionFormat.BGE,
		(func3Wire === "b111".U & opcodeWire === "b1100011".U).asBool -> InstructionFormat.BGEU,

		// Load and Store Instructions
		(func3Wire === "b010".U && opcodeWire === "b0000011".U).asBool -> InstructionFormat.LW,
		(func3Wire === "b001".U && opcodeWire === "b0000011".U).asBool -> InstructionFormat.LH,
		(func3Wire === "b101".U && opcodeWire === "b0000011".U).asBool -> InstructionFormat.LHU,
		(func3Wire === "b000".U && opcodeWire === "b0000011".U).asBool -> InstructionFormat.LB,
		(func3Wire === "b100".U && opcodeWire === "b0000011".U).asBool -> InstructionFormat.LBU,

		(func3Wire === "b010".U & opcodeWire === "b0100011".U).asBool -> InstructionFormat.SW,
		(func3Wire === "b001".U & opcodeWire === "b0100011".U).asBool -> InstructionFormat.SH,
		(func3Wire === "b000".U & opcodeWire === "b0100011".U).asBool -> InstructionFormat.SB
	))

	val instructionTypeWire = MuxCase(InstructionType.NOP, Seq(

		// Normal-Instructions
		
		/* Integer Computational Instructions */
		// Integer Register-Immediate Instructions
		(func3Wire === "b000".U && opcodeWire === "b0010011".U) -> InstructionType.I,
		(func3Wire === "b010".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(func3Wire === "b011".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(func3Wire === "b111".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(func3Wire === "b110".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(func3Wire === "b100".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(cmdWire(31,26) === "b000000".U & func3Wire === "b001".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(cmdWire(31,26) === "b000000".U & func3Wire === "b101".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(cmdWire(31,26) === "b010000".U & func3Wire === "b101".U & opcodeWire === "b0010011".U).asBool -> InstructionType.I,
		(opcodeWire === "b0110111".U) -> InstructionType.U,
		(opcodeWire === "b0010111".U) -> InstructionType.U,
		// Integer Register-Register Operations
		(func7Wire === "b0000000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0000000".U && func3Wire === "b010".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0000000".U && func3Wire === "b011".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0000000".U && func3Wire === "b111".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0000000".U && func3Wire === "b110".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0000000".U && func3Wire === "b100".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0000000".U && func3Wire === "b001".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0000000".U && func3Wire === "b101".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0100000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U) -> InstructionType.R,
		(func7Wire === "b0100000".U && func3Wire === "b101".U && opcodeWire === "b0110011".U) -> InstructionType.R,

		/* Control Transfer Instructions */
		// Unconditional Jumps
		(opcodeWire === "b1101111".U).asBool -> InstructionType.J,
		(func3Wire === "b000".U & opcodeWire === "b1100111".U).asBool -> InstructionType.I,
		// Conditional Branches
		(func3Wire === "b000".U & opcodeWire === "b1100011".U).asBool -> InstructionType.B,
		(func3Wire === "b001".U & opcodeWire === "b1100011".U).asBool -> InstructionType.B,
		(func3Wire === "b100".U & opcodeWire === "b1100011".U).asBool -> InstructionType.B,
		(func3Wire === "b110".U & opcodeWire === "b1100011".U).asBool -> InstructionType.B,
		(func3Wire === "b101".U & opcodeWire === "b1100011".U).asBool -> InstructionType.B,
		(func3Wire === "b111".U & opcodeWire === "b1100011".U).asBool -> InstructionType.B,

		// Load and Store Instructions
		(func3Wire === "b010".U && opcodeWire === "b0000011".U).asBool -> InstructionType.I,
		(func3Wire === "b001".U && opcodeWire === "b0000011".U).asBool -> InstructionType.I,
		(func3Wire === "b101".U && opcodeWire === "b0000011".U).asBool -> InstructionType.I,
		(func3Wire === "b000".U && opcodeWire === "b0000011".U).asBool -> InstructionType.I,
		(func3Wire === "b100".U && opcodeWire === "b0000011".U).asBool -> InstructionType.I,

		(func3Wire === "b010".U & opcodeWire === "b0100011".U).asBool -> InstructionType.S,
		(func3Wire === "b001".U & opcodeWire === "b0100011".U).asBool -> InstructionType.S,
		(func3Wire === "b000".U & opcodeWire === "b0100011".U).asBool -> InstructionType.S
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
		// Pseudo-Instructions
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
		is(InstructionFormat.NOP) {
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
		

		// Normal-Instructions
		/* Integer Computational Instructions */
		// Integer Register-Immediate Instructions
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
		is(InstructionFormat.SLTI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0010".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}		
		is(InstructionFormat.SLTIU) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b1010".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.ANDI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0111".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}	
		is(InstructionFormat.ORI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0110".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}	
		is(InstructionFormat.XORI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0100".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}	
		is(InstructionFormat.SLLI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0001".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}	
		is(InstructionFormat.SRLI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0101".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}	
		is(InstructionFormat.SRAI) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b1101".U
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
		// Integer Register-Register Operations
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
		is(InstructionFormat.SLT) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b0010".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.AND) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b0111".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.OR) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b0110".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.XOR) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b0100".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.SLL) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b0001".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.SRL) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b0101".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.SUB) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b1000".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.SRA) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b1101".U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}

		/* Control Transfer Instructions */
		// Unconditional Jumps
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
		// Conditional Branches
		is(InstructionFormat.BEQ) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= 2.U
			io.branch 	:= "b100".U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.BNE) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= 2.U
			io.branch 	:= "b101".U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.BLT) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= 2.U
			io.branch 	:= "b110".U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.BLTU) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b1010".U
			io.branch 	:= "b110".U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.BGE) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b0010".U
			io.branch 	:= "b111".U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		is(InstructionFormat.BGEU) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 0.U
			io.ctrALU 	:= "b1010".U
			io.branch 	:= "b111".U
			io.memToReg := 0.U
			io.memWR 	:= 0.U
			io.memValid := 0.U
			io.memOP 	:= 0.U
		}
		
		// Load and Store Instructions
		is(InstructionFormat.LW) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0000".U
			io.branch 	:= "b000".U
			io.memToReg := 1.U
			io.memWR 	:= 0.U
			io.memValid := 1.U
			io.memOP 	:= "b010".U
		}
		is(InstructionFormat.LH) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0000".U
			io.branch 	:= "b000".U
			io.memToReg := 1.U
			io.memWR 	:= 0.U
			io.memValid := 1.U
			io.memOP 	:= "b001".U
		}
		is(InstructionFormat.LHU) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0000".U
			io.branch 	:= "b000".U
			io.memToReg := 1.U
			io.memWR 	:= 0.U
			io.memValid := 1.U
			io.memOP 	:= "b101".U
		}
		is(InstructionFormat.LB) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0000".U
			io.branch 	:= "b000".U
			io.memToReg := 1.U
			io.memWR 	:= 0.U
			io.memValid := 1.U
			io.memOP 	:= "b000".U
		}
		is(InstructionFormat.LBU) {
			io.regWR 	:= 1.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= "b01".U
			io.ctrALU 	:= "b0000".U
			io.branch 	:= "b000".U
			io.memToReg := 1.U
			io.memWR 	:= 0.U
			io.memValid := 1.U
			io.memOP 	:= "b100".U
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
		is(InstructionFormat.SH) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 1.U
			io.ctrALU 	:= 0.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 1.U
			io.memValid := 1.U
			io.memOP 	:= 1.U
		}
		is(InstructionFormat.SB) {
			io.regWR 	:= 0.U
			io.srcAALU 	:= 0.U
			io.srcBALU 	:= 1.U
			io.ctrALU 	:= 0.U
			io.branch 	:= 0.U
			io.memToReg := 0.U
			io.memWR 	:= 1.U
			io.memValid := 1.U
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
