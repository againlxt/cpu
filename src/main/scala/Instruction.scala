package singlecyclecpu

import chisel3._
import chisel3.util._

object NpcState extends ChiselEnum {
	val RUNNING, END, ABORT, QUIT, STOP = Value
}

object InstructionFormat extends ChiselEnum {
	val ADD, ADDI,
		NOP = Value
}

object InstructionType extends ChiselEnum {
	val R, I, S, B, U, J,
		NOP = Value
}
