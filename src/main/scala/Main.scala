package cpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import cpu._
import ifu._
import idu._
import exu._
import alu._
import common._
import wbu.WBU
import memory._
import device._
import _root_.interface._
import java.time.Clock

object Main extends App {
	emitVerilog(new top, Array(/*"--split-verilog" ,*/"--target-dir", "generated"))
}

class top extends Module {
	val io = IO(new Bundle {
		val interrupt 	= Input(UInt(1.W))
		val master 	= new AXI
		val slave 	= Flipped(new AXI)
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
	pc.io.wbu2PC 	<> wbu.io.wbu2PC
	val pcWire 		= pc.io.pc

	/* IFU */
	/* Input */
	ifu.io.pc 		:= pcWire
	/* Output */
	ifu.io.inst 			<> idu.io.inst
	ifu.io.ifu2Mem <> xbarAXI.io.axiSlaveIFU

	/* IDU */
	idu.io.idu2EXU 		<> exu.io.idu2EXU
	idu.io.idu2BaseReg	<> riscv32BaseReg.io.idu2BaseReg

	/* EXU */
	exu.io.exu2WBU	<> wbu.io.exu2WBU
	exu.io.exu2CSR 	<> csrReg.io.exu2CSR

	/* WBU */
	wbu.io.wbu2CSR			<> csrReg.io.wbu2CSR
	wbu.io.wbu2BaseReg		<> riscv32BaseReg.io.wbu2BaseReg
	wbu.io.wbu2Mem 			<> xbarAXI.io.axiSlaveWBU

	/* Device */
	/* Peripherals */
	io.master 			<> xbarAXI.io.axiMasterDevice
	AXIUtils.initializeAXISlave(io.slave)
	/* Clint */
	val axiLiteClint = Module(new AXILiteClint)
	axiLiteClint.io.axiLiteMaster 	<> xbarAXI.io.axiLiteClint

	/* DPI-C */
	val getCurPC	= Module(new GetCurPC)
	val getNextPC 	= Module(new GetNextPC)
	getCurPC.io.pc 		:= pcWire
	getNextPC.io.nextPC	:= wbu.io.wbu2PC.bits.nextPC
}

class GetCurPC extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val pc = Input(UInt(32.W))
	})
  setInline("GetCurPC.sv",
	"""module GetCurPC(
	   |  input [31:0] pc 
	   |);
	   |
	   |export "DPI-C" function get_cur_pc;
	   |function bit [31:0] get_cur_pc;
	   |	return pc;
	   |endfunction
	   |endmodule
	""".stripMargin)
}

class GetNextPC extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val nextPC = Input(UInt(32.W))
	})
  setInline("GetNextPC.sv",
	"""module GetNextPC(
	   |  input	[31:0] nextPC
	   |);
	   |
	   |export "DPI-C" function get_next_pc;
	   |function bit [31:0] get_next_pc;
	   |	return nextPC;
	   |endfunction
	   |endmodule
	""".stripMargin)
}
