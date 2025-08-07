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
import dpic.PerformanceCounter
import dpic.PerformanceCounterType
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
	val s_flow :: s_raw :: s_raw_end :: s_flush :: Nil = Enum(4)
	val nextState = WireInit(s_flow)
	val state = RegNext(nextState, s_flow)
	val flushWire 		= exu.io.flush | wbu.io.flush
	val flushEndWire 	= exu.io.exu2LSU.ready & exu.io.exu2LSU.valid
	/* Bypass */
	val bypassRd 		= Wire(Vec(3, UInt(4.W)))
	val bypassData		= Wire(Vec(3, UInt(32.W)))
	val bypassRegWR 	= Wire(Vec(3, Bool()))
	val bypassValid 	= Wire(Vec(3, Bool()))
	bypassRd(0)		:= exu.io.exu2LSU.bits.inst(11,7)
	bypassRd(1)		:= lsu.io.lsu2WBU.bits.inst(11,7)
	bypassRd(2)		:= wbu.io.wbu2BaseReg.rdIndex
	bypassData(0)	:= MuxCase(	0.U(32.W), Seq(	
        (exu.io.exu2LSU.bits.toReg === "b00".U).asBool -> exu.io.exu2LSU.bits.aluData,
		(exu.io.exu2LSU.bits.toReg === "b10".U).asBool -> exu.io.exu2LSU.bits.csrData
    ))
	bypassData(1)	:= MuxCase(	0.U(32.W), Seq(	
        (lsu.io.lsu2WBU.bits.toReg === "b00".U).asBool -> lsu.io.lsu2WBU.bits.aluData,
		(lsu.io.lsu2WBU.bits.toReg === "b01".U).asBool -> lsu.io.lsu2WBU.bits.memData,
		(lsu.io.lsu2WBU.bits.toReg === "b10".U).asBool -> lsu.io.lsu2WBU.bits.toReg
    ))
	bypassData(2)	:= wbu.io.wbu2BaseReg.data
	bypassRegWR(0)	:= exu.io.exu2LSU.bits.regWR
	bypassRegWR(1)	:= lsu.io.lsu2WBU.bits.regWR
	bypassRegWR(2)	:= wbu.io.wbu2BaseReg.regWR
	bypassValid(0)	:= (bypassRd(0) =/= 0.U)
	bypassValid(1)	:= lsu.io.bypassValid & (bypassRd(1) =/= 0.U)
	bypassValid(2)	:= (bypassRd(2) =/= 0.U)
	/* RAW */
	def conflict(rs: UInt, rd: UInt) = ((rs === rd) & (rd =/= 0.U) & (rs =/= 0.U)
	& (!((rs === bypassRd(0)) & (bypassRegWR(0) & bypassValid(0))))
	& (!((rs === bypassRd(1)) & (bypassRegWR(1) & bypassValid(1))))
	& (!((rs === bypassRd(2)) & (bypassRegWR(2) & bypassValid(2)))))
	def conflictWithStage(rs1: UInt, rs2: UInt, rd: UInt) = {
		conflict(rs1, rd) || conflict(rs2, rd)
	}
	val isRAW = Wire(Bool())
	isRAW 	  := 
	(conflictWithStage(idu.io.idu2BaseReg.rs1Index, idu.io.idu2BaseReg.rs2Index, exu.io.rd)) ||
	(conflictWithStage(idu.io.idu2BaseReg.rs1Index, idu.io.idu2BaseReg.rs2Index, lsu.io.rd)) ||
	(conflictWithStage(idu.io.idu2BaseReg.rs1Index, idu.io.idu2BaseReg.rs2Index, wbu.io.rd))
	def pipelineConnect[T <: Data, T2 <: Data](prevOut: DecoupledIO[T],
	thisIn: DecoupledIO[T]) = {
		prevOut.ready 	:= thisIn.ready
		thisIn.bits 	:= RegEnable(prevOut.bits, prevOut.valid & thisIn.ready)
		thisIn.valid 	:= prevOut.valid & thisIn.ready
	}
	nextState := MuxLookup(state, s_flow)(List(
		s_flow	-> Mux(flushWire, s_flush, Mux(isRAW, s_raw, s_flow)),
		s_raw 	-> Mux(flushWire, s_flush, Mux(idu.io.idu2EXU.valid & idu.io.idu2EXU.ready, s_raw_end, s_raw)),
		s_raw_end -> Mux(flushWire, s_flush, Mux(idu.io.inst.valid & idu.io.inst.ready, s_flow, s_raw_end)),
		s_flush	-> Mux(flushEndWire, s_flow, s_flush)
	))	
	pipelineConnect(ifu.io.inst, idu.io.inst)
	pipelineConnect(idu.io.idu2EXU, exu.io.idu2EXU)
	pipelineConnect(exu.io.exu2LSU, lsu.io.exu2LSU)
	pipelineConnect(lsu.io.lsu2WBU, wbu.io.lsu2WBU)
	ifu.io.flush 			:= flushWire
	ifu.io.correctPC 		:= Mux(wbu.io.flush, wbu.io.correctPC, Mux(exu.io.flush, exu.io.currentPC, 0.U))
	idu.io.isRAW 			:= (isRAW & ((state === s_flow) | (state === s_raw)))
	idu.io.flush 			:= flushWire
	idu.io.iduBypass.rd		:= bypassRd
	idu.io.iduBypass.data 	:= bypassData
	idu.io.iduBypass.regWR	:= bypassRegWR
	idu.io.iduBypass.Valid	:= bypassValid
	exu.io.flushing			:= (state === s_flush)
	exu.io.ecallFlush		:= wbu.io.flush
	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val rawCnt = RegInit(0.U(32.W))
		when(nextState === s_raw) {
			rawCnt := rawCnt + 1.U
		} .otherwise {
			rawCnt := 0.U
		}
		val RAWC 			= Module(new PerformanceCounter)
		RAWC.io.valid		:= (state === s_raw) & (nextState === s_raw_end)
		RAWC.io.counterType	:= PerformanceCounterType.RAWCNT.asUInt
		RAWC.io.data		:= rawCnt

		val flushCnt = RegInit(0.U(32.W))
		val EFC	= Module(new PerformanceCounter)
		when(nextState === s_flush) {
			flushCnt := flushCnt + 1.U;
		} .otherwise {
			flushCnt := 0.U
		}
		EFC.io.valid 		:= (state === s_flush) & (nextState === s_flow)
		EFC.io.counterType	:= PerformanceCounterType.FLUSHCNT.asUInt
		EFC.io.data 		:= flushCnt
	}

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

class BranchCond extends Module {
	val io = IO(new Bundle {
		// Input
		val branch 	= Input(UInt(4.W))
		val less 	= Input(Bool())
		val zero 	= Input(Bool())

		// Output
		val pcASrc 	= Output(UInt(2.W))
		val pcBSrc 	= Output(UInt(2.W))
	})

	val branchWire 	= io.branch
	val lessWire 	= io.less
	val zeroWire 	= io.zero

	io.pcASrc 	:= MuxCase (0.U, Seq(
		(branchWire === "b0000".U).asBool -> 0.U,
		(branchWire === "b0001".U).asBool -> 1.U,
		(branchWire === "b0010".U).asBool -> 1.U,
		(branchWire === "b0100".U & !zeroWire).asBool -> 0.U,
		(branchWire === "b0100".U & zeroWire).asBool -> 1.U,
		(branchWire === "b0101".U & !zeroWire).asBool -> 1.U,
		(branchWire === "b0101".U & zeroWire).asBool -> 0.U,
		(branchWire === "b0110".U & !lessWire).asBool -> 0.U,
		(branchWire === "b0110".U & lessWire).asBool -> 1.U,
		(branchWire === "b0111".U & !lessWire).asBool -> 1.U,
		(branchWire === "b0111".U & lessWire).asBool -> 0.U,
		(branchWire === "b1000".U).asBool -> 2.U
	))

	io.pcBSrc := MuxCase(0.U, Seq(
		(branchWire === "b1000".U) -> 2.U,  // 最高优先级
		(branchWire === "b0010".U) -> 1.U,  // 只有b0010返回1
		(branchWire =/= "b1000".U) -> 0.U  // 其他情况返回0
	))	
}

class BranchCheck extends Module {
	val io = IO(new Bundle {
		val predictPC 	= Input(UInt(32.W))
		val correctPC	= Input(UInt(32.W))
		val correct 	= Output(Bool())
	})


	io.correct := Mux((io.predictPC === io.correctPC) | 
	(io.predictPC === 0.U) | (io.correctPC === 4.U), 1.B, 0.B)
}
