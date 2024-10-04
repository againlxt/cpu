package common

import chisel3._
import chisel3.util._

object NpcState extends ChiselEnum{
	val RUNNING, END, ABORT, QUIT, STOP, INIT = Value
}

object InstructionFormat extends ChiselEnum {
	val // Pseudo-instructions
		RET,
		
		// riscv32e instructions
		LUI, AUIPC, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU,
		LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, SLTI,
		SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI, ADD, SUB, SLL,
		SLT, SLTU, XOR, SRL, SRA, OR, AND,

		FENCE, FENCEI, ECALL, MRET, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI,

		// CSR Instructions
		NOP = Value
}

object InstructionType extends ChiselEnum {
	val R, I, S, B, U, J,
		NOP = Value
}
