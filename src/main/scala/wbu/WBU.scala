package wbu

import chisel3._
import chisel3.util._
import memory._
import _root_.interface._
import basemode.AXIAccessFault
import dataclass.data
import basemode.LFSR
import cpu.Config
import dpic._

class WBU extends Module {
	val io = IO(new Bundle {
		val lsu2WBU 	= Flipped(Decoupled(new LSU2WBU))	
        val wbu2CSR     = new WBU2CSR
        val wbu2BaseReg = new WBU2BaseReg
        val wbu2PC      = Decoupled(new WBU2PC)
		val wbu2Icache	= Output(Bool())
	})

    val pcWire 			= io.lsu2WBU.bits.pc
    val memDataWire		= io.lsu2WBU.bits.memData
    val aluDataWire		= io.lsu2WBU.bits.aluData
    val csrWDataWire	= io.lsu2WBU.bits.csrWData
    val csrDataWire		= io.lsu2WBU.bits.csrData
    val immDataWire 	= io.lsu2WBU.bits.immData
    val rs1DataWire 	= io.lsu2WBU.bits.rs1Data
    val instWire 		= io.lsu2WBU.bits.inst
    val regWRWire       = io.lsu2WBU.bits.regWR
    val toRegWire 		= io.lsu2WBU.bits.toReg
    val branchCtrWire 	= io.lsu2WBU.bits.branchCtr
    val lessWire 		= io.lsu2WBU.bits.less
    val zeroWire 		= io.lsu2WBU.bits.zero
    val ecallWire 		= io.lsu2WBU.bits.ecall
    val csrEnWire 		= io.lsu2WBU.bits.csrEn
    val csrWrWire 		= io.lsu2WBU.bits.csrWr
    val fenceiWire		= io.lsu2WBU.bits.fencei

	// state
	val s_idle :: s_wb :: Nil = Enum(2)
	val state 	= RegInit(s_idle)
	val wb_end	= io.wbu2PC.ready & io.wbu2PC.valid
	state 		:= MuxLookup(state, s_idle)(List(
		s_idle	-> Mux(io.lsu2WBU.ready & io.lsu2WBU.valid, s_wb, s_idle),
		s_wb	-> Mux(wb_end, s_idle, s_wb)
	))

	// Branch Cond
	val branchCond 		= Module(new BranchCond)
	// Input
	branchCond.io.branch:= branchCtrWire
	branchCond.io.less 	:= lessWire
	branchCond.io.zero 	:= zeroWire
	// Output
	val pcASrcWire 		= branchCond.io.pcASrc
	val pcBSrcWire 		= branchCond.io.pcBSrc
	
	/* Output */
	io.lsu2WBU.ready	:= (state === s_idle)
    io.wbu2CSR.pc       := pcWire
    io.wbu2CSR.csrWData := csrWDataWire
    io.wbu2CSR.csr      := instWire(31,20)
    io.wbu2CSR.ecall    := ecallWire
    io.wbu2CSR.csrEn    := csrEnWire
    io.wbu2CSR.csrWr    := csrWrWire
    io.wbu2BaseReg.data := MuxCase(	0.U(32.W), Seq(	
        (toRegWire === "b00".U).asBool -> aluDataWire,
		(toRegWire === "b01".U).asBool -> memDataWire,
		(toRegWire === "b10".U).asBool -> csrDataWire
    ))
    io.wbu2BaseReg.rdIndex  := instWire(11,7)
    io.wbu2BaseReg.regWR    := regWRWire
	io.wbu2PC.valid			:= (state === s_wb)
    io.wbu2PC.bits.nextPC   := MuxCase(	0.U(32.W), Seq(	
        (pcASrcWire === "b00".U).asBool	-> 4.U,
		(pcASrcWire === "b01".U).asBool -> immDataWire,
		(pcASrcWire === "b10".U).asBool -> 0.U
    )) + MuxCase(	0.U(32.W), Seq(	
        (pcBSrcWire === "b00".U).asBool	-> pcWire,
		(pcBSrcWire === "b01".U).asBool -> rs1DataWire,
		(pcBSrcWire === "b10".U).asBool -> csrWDataWire
    ))

	io.wbu2Icache	:= fenceiWire
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

	io.pcBSrc 	:= MuxCase (0.U, Seq(
		(branchWire === "b0000".U).asBool -> 0.U,
		(branchWire === "b0001".U).asBool -> 0.U,
		(branchWire === "b0010".U).asBool -> 1.U,
		(branchWire === "b0100".U & !zeroWire).asBool -> 0.U,
		(branchWire === "b0100".U & zeroWire).asBool -> 0.U,
		(branchWire === "b0101".U & !zeroWire).asBool -> 0.U,
		(branchWire === "b0101".U & zeroWire).asBool -> 0.U,
		(branchWire === "b0110".U & !lessWire).asBool -> 0.U,
		(branchWire === "b0110".U & lessWire).asBool -> 0.U,
		(branchWire === "b0111".U & !lessWire).asBool -> 0.U,
		(branchWire === "b1000".U).asBool -> 2.U
	))
}
