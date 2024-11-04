package singlecyclecpu

import chisel3._
import chisel3.util._
import singlecyclecpu._
import ifu._
import idu._
import exu._
import alu._
import common._
import wbu.WBU
import memory._

object Main extends App {
	emitVerilog(new top, Array("--split-verilog" ,"--target-dir", "generated"))
}

class top extends Module {
	val io = IO(new Bundle {
		val npcState 	= Input(UInt(3.W))
		//val memData 	= Input(UInt(32.W))
		val curPC 		= Output(UInt(32.W))
		val nextPC 		= Output(UInt(32.W))
	})
	val pc 				= Module(new PC)
	val ifu 			= Module(new IFU)
	val riscv32BaseReg 	= Module(new Riscv32BaseReg)
	val csrReg 			= Module(new CSRReg)
	val idu 			= Module(new IDU)
	val exu 			= Module(new EXU)
	val wbu 			= Module(new WBU)
	val axiLiteBusArbiter	= Module(new AXILiteBusArbiter)
	//val memDataWire 	= io.memData

	// PC Reg
	pc.io.npcState 	:= io.npcState
	pc.io.wbu2PC 	<> wbu.io.wbu2PC
	val pcWire 		= pc.io.pc
	io.curPC     	:= pcWire
	io.nextPC 		:= wbu.io.wbu2PC.bits.nextPC

	// IFU
	// Input
	ifu.io.pc 		:= pcWire
	//ifu.io.memData 	:= memDataWire
	// Output
	ifu.io.inst 		<> idu.io.inst
	axiLiteBusArbiter.io.axiLiteMaster0	<> ifu.io.ifu2Mem

	// IDU
	idu.io.idu2EXU 		<> exu.io.idu2EXU
	idu.io.idu2BaseReg	<> riscv32BaseReg.io.idu2BaseReg

	// EXU
	exu.io.exu2WBU	<> wbu.io.exu2WBU
	exu.io.exu2CSR 	<> csrReg.io.exu2CSR

	// WBU
	wbu.io.wbu2CSR		<> csrReg.io.wbu2CSR
	wbu.io.wbu2BaseReg	<> riscv32BaseReg.io.wbu2BaseReg
	axiLiteBusArbiter.io.axiLiteMaster1	<> wbu.io.wbu2Mem

	/* Memory */
	val dataSramAXILite					= Module(new AXILiteSram(0.B))
	dataSramAXILite.io.axiLiteM	<> axiLiteBusArbiter.io.axiLiteSlave
}
