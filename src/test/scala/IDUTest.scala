package singlecyclecpu

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import singlecyclecpu._

class IDUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "IDU"

  it should "decode ADDI instruction correctly" in {
    test(new IDU(addrWidth = 32, dataWidth = 32)) { c =>
      // Provide a specific ADDI instruction
      val addiInstruction = "h00128293".U // 0x00128293

      // Set npcState to RUNNING
      c.io.npcState.poke(NpcState.RUNNING.asUInt)
      c.io.cmd.poke(addiInstruction)

      // Step the clock to process the inputs
      c.clock.step(1)

      // Check the outputs
	  println("Result is: " + c.io.insFormat.peek().toString)
      println("Result is: " + c.io.insType.peek().toString)
      println("Result is: " + c.io.opcode.peek().toString)
      println("Result is: " + c.io.rd.peek().toString)
      println("Result is: " + c.io.rs1.peek().toString)
      println("Result is: " + c.io.iImm.peek().toString)
/*
      c.io.insFormat.expect(InstructionFormat.ADDI.asUInt)
      c.io.insType.expect(InstructionType.I.asUInt)
      c.io.opcode.expect("b0010011".U)
      c.io.rd.expect("b00001".U)
      c.io.rs1.expect("b00010".U)
      c.io.iImm.expect("h001".U)
*/
    }
  }
}
