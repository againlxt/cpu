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
import device._
import _root_.interface._

object Main extends App {
	emitVerilog(new top, Array("--split-verilog" ,"--target-dir", "generated"))
}

class top extends Module {
	val io = IO(new Bundle {
		val interrupt 	= Input(UInt(1.W))
		val axiMaster 	= new AXIMaster
		val axiSlave 	= new AXISlave

		val npcState 	= Input(UInt(3.W))
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
	val xbarAXI			= Module(new XbarAXI)

	/* PC Reg */
	pc.io.npcState 	:= io.npcState
	pc.io.wbu2PC 	<> wbu.io.wbu2PC
	val pcWire 		= pc.io.pc
	io.curPC     	:= pcWire
	io.nextPC 		:= wbu.io.wbu2PC.bits.nextPC

	/* IFU */
	/* Input */
	ifu.io.pc 		:= pcWire
	/* Output */
	ifu.io.inst 			<> idu.io.inst
	AXIUtils.connectAXI(ifu.io.ifu2Mem, xbarAXI.io.axiSlaveIFU)

	/* IDU */
	idu.io.idu2EXU 		<> exu.io.idu2EXU
	idu.io.idu2BaseReg	<> riscv32BaseReg.io.idu2BaseReg

	/* EXU */
	exu.io.exu2WBU	<> wbu.io.exu2WBU
	exu.io.exu2CSR 	<> csrReg.io.exu2CSR

	/* WBU */
	wbu.io.wbu2CSR			<> csrReg.io.wbu2CSR
	wbu.io.wbu2BaseReg		<> riscv32BaseReg.io.wbu2BaseReg
	AXIUtils.connectAXI(wbu.io.wbu2Mem, xbarAXI.io.axiSlaveWBU)

	/* Memory */
	val dataSramAXILite				= Module(new AXILiteSram(0.B))
	dataSramAXILite.io.axiLiteM		<> xbarAXI.io.axiLiteSram 	

	/* Device */
	/* Uart */
	val axiLiteUart = Module(new AXILiteUart)
	axiLiteUart.io.axiLiteMaster 	<> xbarAXI.io.axiLiteUart
	/* Clint */
	val axiLiteClint = Module(new AXILiteClint)
	axiLiteClint.io.axiLiteMaster 	<> xbarAXI.io.axiLiteClint
}
