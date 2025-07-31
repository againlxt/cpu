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
		val flush		= Output(Bool())
		val correctPC 	= Output(UInt(32.W))
	})

	val pcReg 		= RegNext(io.idu2EXU.bits.pc) 
	val rs1DataReg 	= RegNext(io.idu2EXU.bits.rs1Data)
	val rs2DataReg 	= RegNext(io.idu2EXU.bits.rs2Data)
	val immReg 		= RegNext(io.idu2EXU.bits.imm)
	val instReg 	= RegNext(io.idu2EXU.bits.inst)

	val regWRReg 	= RegNext(io.idu2EXU.bits.regWR)
	val srcAALUReg 	= RegNext(io.idu2EXU.bits.srcAALU)
	val srcBALUReg 	= RegNext(io.idu2EXU.bits.srcBALU)
	val ctrALUReg 	= RegNext(io.idu2EXU.bits.ctrALU)
	val branchReg 	= RegNext(io.idu2EXU.bits.branch)
	val toRegReg 	= RegNext(io.idu2EXU.bits.toReg)
	val memWRReg 	= RegNext(io.idu2EXU.bits.memWR)
	val memValidReg	= RegNext(io.idu2EXU.bits.memValid)
	val memOPReg 	= RegNext(io.idu2EXU.bits.memOP)
	val rs1IndexReg	= RegNext(io.idu2EXU.bits.rs1Index)
	val ecallReg 	= RegNext(io.idu2EXU.bits.ecall)
	val mretReg 	= RegNext(io.idu2EXU.bits.mret)
	val csrEnReg 	= RegNext(io.idu2EXU.bits.csrEn)
	val csrWrReg 	= RegNext(io.idu2EXU.bits.csrWr)
	val csrOPReg 	= RegNext(io.idu2EXU.bits.csrOP)
	val csrALUOPReg	= RegNext(io.idu2EXU.bits.csrALUOP)

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
	csrALU.io.srcBData 	:= MuxCase(rs1IndexWire, Seq(
		(csrOPWire === 1.U).asBool	-> rs1IndexWire,
		(csrOPWire === 0.U).asBool	-> rs1DataWire
	))
	csrALU.io.csrALUOP 	:= csrALUOPWire
	val csrODataWire= csrALU.io.oData

	/* Branch */
	val branchCond 		= Module(new BranchCond)
	val branchCheck		= Module(new BranchCheck)
	branchCond.io.branch	:= branchCtrWire
	branchCond.io.less 		:= lessWire
	branchCond.io.zero 		:= zeroWire
	val pcASrcWire 		= branchCond.io.pcASrc
	val pcBSrcWire 		= branchCond.io.pcBSrc
    val nextPC  		= MuxCase(0.U(32.W), Seq(	
        (pcASrcWire === "b00".U).asBool	-> 4.U,
		(pcASrcWire === "b01".U).asBool  -> immDataWire,
		(pcASrcWire === "b10".U).asBool  -> 0.U
    )) + MuxCase(	0.U(32.W), Seq(	
        (pcBSrcWire === "b00".U).asBool  -> pcWire,
		(pcBSrcWire === "b01".U).asBool  -> rs1DataWire,
		(pcBSrcWire === "b10".U).asBool  -> csrODataWire
    ))
	branchCheck.io.predictPC 	:= io.idu2EXU.bits.pc
	branchCheck.io.correctPC	:= nextPC
	val branchCorrect 			= branchCheck.io.correct

	val handReg 	= RegNext(io.idu2EXU.ready & io.idu2EXU.valid & branchCorrect)

	/* State */
	val s_idle :: s_flush :: Nil = Enum(2)
	val state 	= RegInit(s_flush)
	state := MuxLookup(state, s_idle)(List(
		s_idle	-> Mux(!branchCorrect, s_flush, s_idle),
		s_flush	-> Mux(io.idu2EXU.ready & io.idu2EXU.valid, s_idle, s_flush)
	))

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val exuFinCalCnt = RegInit(0.U(32.W))
		when (io.idu2EXU.valid & io.idu2EXU.ready) {
			exuFinCalCnt := 1.U
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
	io.exu2LSU.bits.ecall 		:= ecallWire
	io.exu2LSU.bits.csrEn 		:= csrEnWire
	io.exu2LSU.bits.csrWr		:= csrWrWire

	io.exu2CSR.csr 				:= instWire(31,20)
	io.exu2CSR.mret 			:= mretWire
	io.exu2CSR.ecall 			:= ecallWire

	io.flush 					:= (!branchCorrect) & io.idu2EXU.ready & io.idu2EXU.valid & (state === s_idle)
	io.correctPC 				:= nextPC

	io.idu2EXU.ready   	:= ((state === s_idle) & branchCorrect) | (state === s_flush)
    io.exu2LSU.valid	:= handReg
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

	io.pcASrc := MuxCase(0.U, Seq(
		(branchWire === "b0000".U) -> 0.U,
		(branchWire === "b0001".U) -> 1.U,
		(branchWire === "b0010".U) -> 1.U,
		(branchWire === "b0100".U) -> zeroWire,
		(branchWire === "b0101".U) -> (!zeroWire).asUInt,
		(branchWire === "b0110".U) -> lessWire,
		(branchWire === "b0111".U) -> (!lessWire).asUInt,
		(branchWire === "b1000".U) -> 2.U
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

	io.correct := Mux((io.predictPC === io.correctPC), 1.B, 0.B)
}
