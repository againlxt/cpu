package cpu 

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq
import common.InstructionType
import _root_.interface._

class Riscv32BaseReg extends Module {
	val io = IO(new Bundle {
		val idu2BaseReg = Flipped(new IDU2BaseReg)
		val exu2BaseReg = Flipped(new EXU2BaseReg)
		val lsu2BaseReg = Flipped(new LSU2BaseReg)
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

	val rawWire0 = 
	((io.idu2BaseReg.rs1Index === io.exu2BaseReg.rdIndex) & io.exu2BaseReg.regWR) | 
	((io.idu2BaseReg.rs1Index === io.lsu2BaseReg.rdIndex) & io.lsu2BaseReg.regWR) |
	((io.idu2BaseReg.rs1Index === io.wbu2BaseReg.rdIndex) & io.wbu2BaseReg.regWR) |
	((io.idu2BaseReg.rs2Index === io.exu2BaseReg.rdIndex) & io.exu2BaseReg.regWR) | 
	((io.idu2BaseReg.rs2Index === io.lsu2BaseReg.rdIndex) & io.lsu2BaseReg.regWR) |
	((io.idu2BaseReg.rs2Index === io.wbu2BaseReg.rdIndex) & io.wbu2BaseReg.regWR)
	val rawWire1 = 
	((io.idu2BaseReg.rs1Index === io.exu2BaseReg.rdIndex) & io.exu2BaseReg.regWR) | 
	((io.idu2BaseReg.rs1Index === io.lsu2BaseReg.rdIndex) & io.lsu2BaseReg.regWR) |
	((io.idu2BaseReg.rs1Index === io.wbu2BaseReg.rdIndex) & io.wbu2BaseReg.regWR)
	val rawWire  = MuxCase(rawWire0, Seq(
		(io.idu2BaseReg.instType === InstructionType.R.asUInt)	-> rawWire0,
		(io.idu2BaseReg.instType === InstructionType.I.asUInt)	-> rawWire1,
		(io.idu2BaseReg.instType === InstructionType.S.asUInt)	-> rawWire0,
		(io.idu2BaseReg.instType === InstructionType.B.asUInt)	-> rawWire0,
		(io.idu2BaseReg.instType === InstructionType.U.asUInt)	-> 0.B,
		(io.idu2BaseReg.instType === InstructionType.J.asUInt)	-> 0.B
	))
	val ifu2IDUHandshakeReg = RegNext(io.idu2BaseReg.handShake)

	val s_idle :: s_wait_exu :: s_wait_lsu :: s_wait_wbu :: s_wait_idu :: Nil = Enum(5)
	val state 	= RegInit(s_idle)
	state := MuxLookup(state, s_idle)(List(
		s_idle		-> Mux(rawWire.asBool & ifu2IDUHandshakeReg, s_wait_exu, s_idle),
		s_wait_exu	-> Mux(io.exu2BaseReg.handShake, s_wait_lsu, s_wait_exu),
		s_wait_lsu	-> Mux(io.lsu2BaseReg.handShake, s_wait_wbu, s_wait_lsu),
		s_wait_wbu	-> s_wait_idu,
		s_wait_idu	-> Mux(io.idu2BaseReg.handShake, s_idle, s_wait_idu)
	))

	io.idu2BaseReg.rs1Data 	:= riscv32BaseReg(io.idu2BaseReg.rs1Index)
	io.idu2BaseReg.rs2Data 	:= riscv32BaseReg(io.idu2BaseReg.rs2Index)
	io.idu2BaseReg.raw 		:= Mux((state === s_idle), rawWire, state =/= s_wait_idu)
}
