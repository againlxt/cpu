package cpu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import cpu._
import ifu._
import lsu._
import idu._
import exu._
import alu._
import common._
import wbu.WBU
import memory._
import device._
import _root_.interface._
import java.time.Clock
import java.awt.MouseInfo
import _root_.basemode.AXIAccessFault
object Main extends App {
	emitVerilog(new top, Array("--emit-modules", "verilog", "--target-dir", "generated"))
}

class top extends Module {
	val io = IO(new Bundle {
		val interrupt 	= Input(UInt(1.W))
		val master 	= if (Config.SoC) Some(new AXI) else None
		//val slave 	= if (Config.SoC) Some(Flipped(new AXI)) else None
		val slave 	= Flipped(new AXI)
	})
	val ifu 			= Module(new IFU)
	val riscv32BaseReg 	= Module(new Riscv32BaseReg)
	val csrReg 			= Module(new CSRReg)
	val idu 			= Module(new IDU)
	val exu 			= Module(new EXU)
	val lsu 			= Module(new LSU)
	val wbu 			= Module(new WBU)
	val xbarAXI			= Module(new XbarAXI)
	val icacheSkidBuffer= Module(new AXISkidBuffer(false, false, false, false, false))

	/* PipeLine */
	def conflict(rs: UInt, rd: UInt) = (rs === rd)
	def conflictWithStage(rs1: UInt, rs2: UInt, rd: UInt) = {
		conflict(rs1, rd) || conflict(rs2, rd)
	}
	val isRAW = Wire(Bool())
	val isRAWReg = RegNext(isRAW)
	val rawPC = RegEnable(exu.io.idu2EXU.bits.pc, isRAW)
	isRAW 	  := 
	(conflictWithStage(idu.io.idu2BaseReg.rs1Index, idu.io.idu2BaseReg.rs2Index, exu.io.rd) & (rawPC =/= exu.io.idu2EXU.bits.pc)) ||
	(conflictWithStage(idu.io.idu2BaseReg.rs1Index, idu.io.idu2BaseReg.rs2Index, lsu.io.rd) & (rawPC =/= lsu.io.exu2LSU.bits.pc)) ||
	(conflictWithStage(idu.io.idu2BaseReg.rs1Index, idu.io.idu2BaseReg.rs2Index, wbu.io.rd) & (rawPC =/= wbu.io.lsu2WBU.bits.pc))
	def pipelineConnect[T <: Data, T2 <: Data](prevOut: DecoupledIO[T],
	thisIn: DecoupledIO[T]) = {
		prevOut.ready 	:= thisIn.ready
		thisIn.bits 	:= RegEnable(prevOut.bits, prevOut.valid & thisIn.ready)
		thisIn.valid 	:= RegEnable(prevOut.valid & thisIn.ready, 1.B)
	}
	pipelineConnect(ifu.io.inst, idu.io.inst)
	pipelineConnect(idu.io.idu2EXU, exu.io.idu2EXU)
	pipelineConnect(exu.io.exu2LSU, lsu.io.exu2LSU)
	pipelineConnect(lsu.io.lsu2WBU, wbu.io.lsu2WBU)
	ifu.io.flush 			:= 0.B
	ifu.io.correctPC 		:= 0.U
	idu.io.isRAW 			:= isRAW | isRAWReg

	/* IFU */
	/* Input */
	val pcWire 		= ifu.io.inst.bits.pc
	/* Output */
	/* Icache */
	val numOfCache 	= 16
	val sizeOfCache	= 128
	val way 		= 4
	val m 			= log2Ceil(sizeOfCache >> 3)
	val n 			= log2Up(numOfCache/way)
	val burstLen	= 4
	val burstSize 	= 16
	val icache = Module(new Icache(numOfCache, sizeOfCache, m, n, burstLen, burstSize, way, ReplacePolicy.LRU))
	icache.io.addr 				:= ifu.io.ifu2Icache.addr
	icache.io.enable			:= ifu.io.ifu2Icache.enable
	ifu.io.ifu2Icache.oEnable	:= icache.io.oEnable
	ifu.io.ifu2Icache.inst		:= icache.io.inst
	icache.io.icache2Mem 		<> icacheSkidBuffer.io.axiSlave
	icacheSkidBuffer.io.axiMaster	<> xbarAXI.io.axiSlaveIFU
	if(Config.hasDPIC & (!Config.isSTA)) {
		val axiAccessFault = Module(new AXIAccessFault())
		axiAccessFault.io.ready := icache.io.icache2Mem.bready
		axiAccessFault.io.valid := icache.io.icache2Mem.bvalid
		axiAccessFault.io.resp	:= icache.io.icache2Mem.bresp
	}
	/* IDU */
	idu.io.idu2BaseReg	<> riscv32BaseReg.io.idu2BaseReg
	/* EXU */
	exu.io.exu2CSR 	<> csrReg.io.exu2CSR
	/* LSU */
	lsu.io.lsu2Mem	<> xbarAXI.io.axiSlaveLSU
	/* WBU */
	wbu.io.wbu2CSR			<> csrReg.io.wbu2CSR
	wbu.io.wbu2BaseReg		<> riscv32BaseReg.io.wbu2BaseReg
	val wbu2Icache 			= wbu.io.wbu2Icache
	icache.io.wbu2Icache	:= wbu2Icache

	/* Device */
	/* Peripherals */
	io.master.foreach { m => m <> xbarAXI.io.axiMasterDevice}
	AXIUtils.initializeAXISlave(io.slave)	
	/* Clint */
	val axiLiteClint = Module(new AXILiteClint)
	axiLiteClint.io.axiLiteMaster 	<> xbarAXI.io.axiLiteClint
	/* Memory */
	if(!Config.SoC) {
		val dataSramAXILite			= Module(new AXILiteSram(0.B))
		AXIUtils.initializeAXISlave(xbarAXI.io.axiMasterDevice)
		xbarAXI.io.axiMasterDevice.awready    := dataSramAXILite.io.axiLiteM.awReady
        dataSramAXILite.io.axiLiteM.awValid      := xbarAXI.io.axiMasterDevice.awvalid
        dataSramAXILite.io.axiLiteM.awAddr       := xbarAXI.io.axiMasterDevice.awaddr
        /* W */
        xbarAXI.io.axiMasterDevice.wready     := dataSramAXILite.io.axiLiteM.wReady
        dataSramAXILite.io.axiLiteM.wValid       := xbarAXI.io.axiMasterDevice.wvalid
        dataSramAXILite.io.axiLiteM.wData        := xbarAXI.io.axiMasterDevice.wdata
        dataSramAXILite.io.axiLiteM.wStrb        := xbarAXI.io.axiMasterDevice.wstrb
        /* B */
        xbarAXI.io.axiMasterDevice.bresp      := dataSramAXILite.io.axiLiteM.bResp
        xbarAXI.io.axiMasterDevice.bvalid     := dataSramAXILite.io.axiLiteM.bValid
        dataSramAXILite.io.axiLiteM.bReady       := xbarAXI.io.axiMasterDevice.bready
        /* AR */
        xbarAXI.io.axiMasterDevice.arready    := dataSramAXILite.io.axiLiteM.arReady
        dataSramAXILite.io.axiLiteM.arValid      := xbarAXI.io.axiMasterDevice.arvalid
        dataSramAXILite.io.axiLiteM.arAddr       := xbarAXI.io.axiMasterDevice.araddr
        /* R */
        xbarAXI.io.axiMasterDevice.rdata      := dataSramAXILite.io.axiLiteM.rData
        xbarAXI.io.axiMasterDevice.rresp      := dataSramAXILite.io.axiLiteM.rrEsp
        xbarAXI.io.axiMasterDevice.rvalid     := dataSramAXILite.io.axiLiteM.rValid
        dataSramAXILite.io.axiLiteM.rReady       := xbarAXI.io.axiMasterDevice.rready
	}
	/* Uart */
	if(!Config.SoC) {
		val axiLiteUart = Module(new AXILiteUart)
		xbarAXI.io.axiLiteUart.foreach {uart => uart <> axiLiteUart.io.axiLiteMaster}
	}
}
