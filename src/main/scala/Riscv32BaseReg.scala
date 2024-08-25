package  singlecyclecpu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq

class Riscv32BaseReg extends Module {
	val io = IO(new Bundle {
		val rs1Index 	= Input(UInt(4.W))
		val rs2Index 	= Input(UInt(4.W))
		val rdIndex 	= Input(UInt(4.W))
		val dataIn 		= Input(UInt(32.W))
		val regWR 		= Input(Bool())

		val rs1Data 	= Output(UInt(32.W))
		val rs2Data 	= Output(UInt(32.W))
	})

	val riscv32BaseReg 	= RegInit(VecInit(Seq.fill(16)(0.U(32.W))))

	when(io.regWR === 1.U) {
		when(io.rdIndex =/= 0.U) {
			riscv32BaseReg(io.rdIndex)	:= io.dataIn
		} .otherwise {
			riscv32BaseReg(0.U) := 0.U
		}
		
	} .otherwise {
		riscv32BaseReg(io.rdIndex) := riscv32BaseReg(io.rdIndex)
	}

	io.rs1Data 	:= riscv32BaseReg(io.rs1Index)
	io.rs2Data 	:= riscv32BaseReg(io.rs2Index)
}
