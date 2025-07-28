package exu

import chisel3._
import chisel3.util._
import exu._
import alu._
import common._
import cpu._
import chisel3.util.HasBlackBoxResource
import java.time.Clock
import _root_.interface._
import cpu.Config
import dpic._

class EXU extends Module {
	val io = IO(new Bundle {
		val idu2EXU 	= Flipped(Decoupled(new IDU2EXU))
		val exu2LSU 	= Decoupled(new EXU2LSU)
		val exu2CSR 	= new EXU2CSR
	})

	val pcReg 		= RegInit(Mux(Config.SoC.asBool, "h30000000".U(32.W), "h80000000".U(32.W))) 
	val rs1DataReg 	= RegInit(0.U(32.W))
	val rs2DataReg 	= RegInit(0.U(32.W))
	val immReg 		= RegInit(0.U(32.W))
	val instReg 	= RegInit(0.U(32.W))

	val regWRReg 	= RegInit(0.U(1.W))
	val srcAALUReg 	= RegInit(0.U(2.W))
	val srcBALUReg 	= RegInit(0.U(2.W))
	val ctrALUReg 	= RegInit(0.U(4.W))
	val branchReg 	= RegInit(0.U(4.W))
	val toRegReg 	= RegInit(0.U(2.W))
	val memWRReg 	= RegInit(0.U(1.W))
	val memValidReg	= RegInit(0.U(1.W))
	val memOPReg 	= RegInit(0.U(3.W))
	val rs1IndexReg	= RegInit(0.U(5.W))
	val ecallReg 	= RegInit(0.U(1.W))
	val mretReg 	= RegInit(0.U(1.W))
	val csrEnReg 	= RegInit(0.U(1.W))
	val csrWrReg 	= RegInit(0.U(1.W))
	val csrOPReg 	= RegInit(0.U(1.W))
	val csrALUOPReg	= RegInit(0.U(2.W))

	val ready2IDUReg= RegInit(1.U(1.W))
    io.idu2EXU.ready   := ready2IDUReg.asBool
    val valid2WBUReg= RegInit(0.U(1.W))
    io.exu2LSU.valid:= valid2WBUReg.asBool

	// State Machine
	val s_idle :: s_wait_idu_valid :: s_wait_wbu_ready :: Nil = Enum(3)
	val state = RegInit(s_idle)
	state := MuxLookup(state, s_idle)(List(
		s_idle				-> Mux(reset.asBool, s_idle, s_wait_idu_valid),
		s_wait_idu_valid	-> Mux(reset.asBool, s_idle, Mux(io.idu2EXU.valid, s_wait_wbu_ready, s_wait_idu_valid)),
		s_wait_wbu_ready	-> Mux(reset.asBool, s_idle, Mux(io.exu2LSU.ready, s_idle, s_wait_wbu_ready))
	))
	// handshake signals control
	when(state === s_idle) {
		ready2IDUReg := 1.U
		valid2WBUReg := 0.U
	} .elsewhen(state === s_wait_idu_valid) {
		ready2IDUReg := 1.U
		valid2WBUReg := 0.U
	} .elsewhen(state === s_wait_wbu_ready) {
		ready2IDUReg := 0.U
		valid2WBUReg := 1.U
	}

	// Data signal storage
	when(io.idu2EXU.ready && io.idu2EXU.valid) {
        pcReg 		:= io.idu2EXU.bits.pc
		rs1DataReg 	:= io.idu2EXU.bits.rs1Data
		rs2DataReg 	:= io.idu2EXU.bits.rs2Data
		immReg 		:= io.idu2EXU.bits.imm
		instReg 	:= io.idu2EXU.bits.inst

		regWRReg 	:= io.idu2EXU.bits.regWR
		srcAALUReg 	:= io.idu2EXU.bits.srcAALU
		srcBALUReg 	:= io.idu2EXU.bits.srcBALU
		ctrALUReg 	:= io.idu2EXU.bits.ctrALU
		branchReg 	:= io.idu2EXU.bits.branch
		toRegReg 	:= io.idu2EXU.bits.toReg
		memWRReg 	:= io.idu2EXU.bits.memWR
		memValidReg	:= io.idu2EXU.bits.memValid
		memOPReg 	:= io.idu2EXU.bits.memOP
		rs1IndexReg	:= io.idu2EXU.bits.rs1Index
		ecallReg 	:= io.idu2EXU.bits.ecall
		mretReg 	:= io.idu2EXU.bits.mret
		csrEnReg 	:= io.idu2EXU.bits.csrEn
		csrWrReg 	:= io.idu2EXU.bits.csrWr
		csrOPReg 	:= io.idu2EXU.bits.csrOP
		csrALUOPReg	:= io.idu2EXU.bits.csrALUOP
    }

	// Wire
	val pcWire			= pcReg
	val rs1DataWire 	= rs1DataReg
	val rs2DataWire 	= rs2DataReg
	val immDataWire 	= immReg
	val instWire 		= instReg
	val regWRWire 		= regWRReg
	val aluASrcCtrWire 	= srcAALUReg
	val aluBSrcCtrWire 	= srcBALUReg
	val aluCtrWire 		= ctrALUReg
	val branchCtrWire 	= branchReg
	val memToRegCtrWire = toRegReg
	val memOPCtrWire 	= memOPReg
	val memWRCtrWire	= memWRReg
	val memValidCtrWire = memValidReg
	val rs1IndexWire 	= rs1IndexReg
	val ecallWire 		= ecallReg
	val mretWire 		= mretReg
	val csrEnWire 		= csrEnReg
	val csrWrWire 		= csrWrReg
	val csrOPWire 		= csrOPReg
	val csrALUOPWire 	= csrALUOPReg
	val csrDataWire 	= io.exu2CSR.csrData

	// ALU
	val srcADataWire 	= MuxCase(0.U(32.W), Seq(
		(aluASrcCtrWire === 0.U).asBool -> rs1DataWire,
		(aluASrcCtrWire === 1.U).asBool -> pcWire,
	))
	val srcBDataWire 	= MuxCase(0.U(32.W), Seq(
		(aluBSrcCtrWire === 0.U).asBool -> rs2DataWire,
		(aluBSrcCtrWire === 1.U).asBool -> immDataWire,
		(aluBSrcCtrWire === 2.U).asBool -> 4.U
	))
	val alu = Module(new ALU)
	// Input
	alu.io.aluCtr 		:= aluCtrWire
	alu.io.srcAData 	:= srcADataWire
	alu.io.srcBData 	:= srcBDataWire
	// Output
	val lessWire 	= alu.io.less
	val zeroWire 	= alu.io.zero
	val resultWire 	= alu.io.aluOut

	val csrALU 		= Module(new CSRALU)
	// Input
	csrALU.io.srcAData	:= csrDataWire
	csrALU.io.srcBData 	:= Mux(csrOPWire.asBool, rs1IndexWire, rs1DataWire)
	csrALU.io.csrALUOP 	:= csrALUOPWire
	val csrODataWire= csrALU.io.oData

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val exuFinCalCnt = RegInit(0.U(32.W))
		when (io.idu2EXU.valid & io.idu2EXU.ready) {
			exuFinCalCnt := 0.U
		} .otherwise {
			exuFinCalCnt := exuFinCalCnt + 1.U
		}
		val EFCC 			= Module(new PerformanceCounter)
		EFCC.io.valid		:= io.exu2LSU.valid
		EFCC.io.counterType	:= PerformanceCounterType.EXUFINCAL.asUInt
		EFCC.io.data 		:= exuFinCalCnt
	}	

	io.exu2LSU.bits.pc 			:= pcWire
	io.exu2LSU.bits.memData		:= rs2DataWire
	io.exu2LSU.bits.aluData 	:= resultWire
	io.exu2LSU.bits.csrWData 	:= csrODataWire
	io.exu2LSU.bits.csrData 	:= csrDataWire
	io.exu2LSU.bits.immData 	:= immDataWire
	io.exu2LSU.bits.rs1Data 	:= rs1DataWire
	io.exu2LSU.bits.inst 		:= instWire

	io.exu2LSU.bits.regWR 		:= regWRWire
	io.exu2LSU.bits.memWR 		:= memWRCtrWire
	io.exu2LSU.bits.memValid	:= memValidCtrWire
	io.exu2LSU.bits.memOP		:= memOPCtrWire
	io.exu2LSU.bits.toReg 		:= memToRegCtrWire
	io.exu2LSU.bits.branchCtr	:= branchCtrWire
	io.exu2LSU.bits.less 		:= lessWire
	io.exu2LSU.bits.zero 		:= zeroWire
	io.exu2LSU.bits.ecall 		:= ecallWire
	io.exu2LSU.bits.csrEn 		:= csrEnWire
	io.exu2LSU.bits.csrWr		:= csrWrWire

	io.exu2CSR.csr 				:= instWire(31,20)
	io.exu2CSR.mret 			:= mretWire
	io.exu2CSR.ecall 			:= ecallWire
}
