package singlecyclecpu

import chisel3._
import chisel3.util._
import common._

class PC extends Module {
	val io = IO(new Bundle {
		val npcState    = Input(UInt(32.W))
		val dnpc 		= Input(UInt(32.W))
		val pc			= Output(UInt(32.W))
	})

	// 使用 RegInit 设置 PC 的初始值为 0x80000000
	val pcReg = RegInit(BigInt("80000000", 16).U(32.W))

	// 使用 RegNext 更新 PC 值
	pcReg := MuxCase(BigInt("80000000", 16).U(32.W), Seq(
		(io.npcState === NpcState.RUNNING.asUInt).asBool -> io.dnpc.asUInt,
		(io.npcState =/= NpcState.RUNNING.asUInt).asBool -> pcReg.asUInt
	))

	// 输出当前 PC 值
	io.pc := pcReg
}
