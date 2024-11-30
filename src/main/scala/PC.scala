package singlecyclecpu

import chisel3._
import chisel3.util._
import common._
import _root_.interface.WBU2PC

class PC extends Module {
	val io = IO(new Bundle {
		val wbu2PC 		= Flipped(Decoupled(new WBU2PC))
		val pc			= Output(UInt(32.W))
	})

	/* 使用 RegInit 设置 PC 的初始值为 0x20000000 */
	val pcReg = RegInit(BigInt("20000000", 16).U(32.W))
	val wbu2PCReadyReg = RegInit(1.U(1.W))
	io.wbu2PC.ready := wbu2PCReadyReg

	/* 使用 RegNext 更新 PC 值 */
	when (io.wbu2PC.ready && io.wbu2PC.valid) {
		pcReg := io.wbu2PC.bits.nextPC.asUInt
		wbu2PCReadyReg := 0.U
	} .otherwise {
		wbu2PCReadyReg := 1.U
	}

	/* 输出当前 PC 值 */
	io.pc := pcReg
}
