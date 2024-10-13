package  singlecyclecpu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq
import _root_.interface.IDU2BaseReg
import _root_.interface.WBU2BaseReg

class Riscv32BaseReg extends Module {
	val io = IO(new Bundle {
		val idu2BaseReg = Flipped(new IDU2BaseReg)
		val wbu2BaseReg = Flipped(new WBU2BaseReg)
	})

	val riscv32BaseReg 	= RegInit(VecInit(Seq.fill(16)(0.U(32.W))))

	when(io.wbu2BaseReg.regWR === 1.U) {
		when(io.wbu2BaseReg.rdIndex =/= 0.U) {
			riscv32BaseReg(io.wbu2BaseReg.rdIndex)	:= io.wbu2BaseReg.data
		} .otherwise {
			riscv32BaseReg(0.U) := 0.U
		}
	}

	io.idu2BaseReg.rs1Data 	:= riscv32BaseReg(io.idu2BaseReg.rs1Index)
	io.idu2BaseReg.rs2Data 	:= riscv32BaseReg(io.idu2BaseReg.rs2Index)
}
