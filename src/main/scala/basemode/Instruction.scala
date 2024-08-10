package common

import chisel3._
import chisel3.util._

object NpcState extends ChiselEnum{
	val RUNNING, END, ABORT, QUIT, STOP, INIT = Value
}

object InstructionFormat extends ChiselEnum {
	val ADD, ADDI, LUI, AUIPC, SLTI,
		JAL, JALR, BEQ,
		SW,
		NOP = Value
}

object InstructionType extends ChiselEnum {
	val R, I, S, B, U, J,
		NOP = Value
}
