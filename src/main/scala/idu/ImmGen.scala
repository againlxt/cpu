package idu

import chisel3._
import chisel3.util._

class ImmGen extends Module {
	val io = IO(new Bundle {
		val iImm 	  = Input(UInt(12.W))
		val sImm      = Input(UInt(12.W))
		val bImm      = Input(UInt(13.W))
		val uImm      = Input(UInt(32.W))
		val jImm      = Input(UInt(21.W))
		val immType   = Input(UInt(3.W))

		val imm 	  = Output(UInt(32.W))
	})

	val iImmWire 	= Cat(0.U(20.W), io.iImm)
	val sImmWire 	= Cat(0.U(20.W), io.sImm)
	val bImmWire 	= Cat(0.U(19.W), io.bImm)
	val uImmWire 	= io.uImm
	val jImmWire 	= Cat(0.U(11.W), io.jImm)
	val immTypewire = io.immType

	io.imm := MuxCase(	0.U(32.W),
						Array(	(immTypewire === 1.U) -> iImmWire,
								(immTypewire === 2.U) -> sImmWire,
								(immTypewire === 3.U) -> bImmWire,
								(immTypewire === 4.U) -> uImmWire,
								(immTypewire === 5.U) -> jImmWire).toIndexedSeq)
}

