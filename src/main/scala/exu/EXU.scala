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
		val rd 			= Output(UInt(4.W))
		val currentPC 	= Output(UInt(32.W))
		val flush 		= Output(Bool())
		val flushing 	= Input(Bool())
	})

	// Wire
	val pcWire			= io.idu2EXU.bits.pc
	val rs1DataWire 	= io.idu2EXU.bits.rs1Data
	val rs2DataWire 	= io.idu2EXU.bits.rs2Data
	val immDataWire 	= io.idu2EXU.bits.imm
	val instWire 		= io.idu2EXU.bits.inst
	val regWRWire 		= io.idu2EXU.bits.regWR
	val aluASrcCtrWire 	= io.idu2EXU.bits.srcAALU
	val aluBSrcCtrWire 	= io.idu2EXU.bits.srcBALU
	val aluCtrWire 		= io.idu2EXU.bits.ctrALU
	val branchCtrWire 	= io.idu2EXU.bits.branch
	val memToRegCtrWire = io.idu2EXU.bits.toReg
	val memOPCtrWire 	= io.idu2EXU.bits.memOP
	val memWRCtrWire	= io.idu2EXU.bits.memWR
	val memValidCtrWire = io.idu2EXU.bits.memValid
	val rs1IndexWire 	= io.idu2EXU.bits.rs1Index
	val ecallWire 		= io.idu2EXU.bits.ecall
	val mretWire 		= io.idu2EXU.bits.mret
	val csrEnWire 		= io.idu2EXU.bits.csrEn
	val csrWrWire 		= io.idu2EXU.bits.csrWr
	val csrOPWire 		= io.idu2EXU.bits.csrOP
	val csrALUOPWire 	= io.idu2EXU.bits.csrALUOP
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
	val predictPCReg 	= RegEnable(nextPC, io.idu2EXU.valid & io.idu2EXU.ready)
	val handReg 		= RegNext(io.idu2EXU.valid & io.idu2EXU.ready)
	val branchCheck 	= Module(new BranchCheck)
	branchCheck.io.predictPC := pcWire
	branchCheck.io.correctPC := predictPCReg
	val flushWire 		= (!branchCheck.io.correct) & (predictPCReg =/= 4.U) & (handReg) & (!io.flushing)

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

	io.rd	:= Mux(regWRWire.asBool, Mux(io.idu2EXU.ready & !io.exu2LSU.valid, 0.U, instWire(11,7)), 0.U)
	io.currentPC	:= predictPCReg
	io.flush 		:= flushWire

	val validReg = RegInit(0.B)
	val readyReg = RegInit(1.B)
	when(!flushWire) {
		switch(validReg) {
			is(0.B) { validReg := io.idu2EXU.valid & io.idu2EXU.ready}
			is(1.B) {
				validReg := Mux(io.exu2LSU.valid & io.exu2LSU.ready,
				Mux(io.idu2EXU.valid & io.idu2EXU.ready, 1.B, 0.B), 1.B)
			}
		}
		switch(readyReg) {
			is(0.B) { readyReg := io.exu2LSU.valid & io.exu2LSU.ready }
			is(1.B) {
				readyReg := Mux(io.idu2EXU.valid & io.idu2EXU.ready, 
				Mux(io.exu2LSU.valid & io.exu2LSU.ready, 1.B, 0.B), 1.B)
		}
	}
	} .otherwise {
		validReg := 0.U
		readyReg := 1.B
	}
	io.idu2EXU.ready   	:= readyReg & (!flushWire)
    io.exu2LSU.valid	:= validReg & (!flushWire)
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

	io.correct := Mux((io.predictPC === io.correctPC), 1.B, 0.B)
}
