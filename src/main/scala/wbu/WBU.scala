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

	val pcReg 			= RegNext(io.lsu2WBU.bits.pc)
    val memDataReg		= RegNext(io.lsu2WBU.bits.memData)
    val aluDataReg		= RegNext(io.lsu2WBU.bits.aluData)
    val csrWDataReg		= RegNext(io.lsu2WBU.bits.csrWData)
    val csrDataReg		= RegNext(io.lsu2WBU.bits.csrData)
    val immDataReg 		= RegNext(io.lsu2WBU.bits.immData)
    val rs1DataReg 		= RegNext(io.lsu2WBU.bits.rs1Data)
    val instReg 		= RegNext(io.lsu2WBU.bits.inst)
    val regWRReg       	= RegNext(io.lsu2WBU.bits.regWR)
    val toRegReg 		= RegNext(io.lsu2WBU.bits.toReg)
    val branchCtrReg 	= RegNext(io.lsu2WBU.bits.branchCtr)
    val lessReg 		= RegNext(io.lsu2WBU.bits.less)
    val zeroReg 		= RegNext(io.lsu2WBU.bits.zero)
    val ecallReg 		= RegNext(io.lsu2WBU.bits.ecall)
    val csrEnReg 		= RegNext(io.lsu2WBU.bits.csrEn)
    val csrWrReg 		= RegNext(io.lsu2WBU.bits.csrWr)
    val fenceiReg		= RegNext(io.lsu2WBU.bits.fencei)
	val handReg 		= RegNext(io.lsu2WBU.valid & io.lsu2WBU.ready)
		
	// Branch Cond
	val branchCond 		= Module(new BranchCond)
	// Input
	branchCond.io.branch:= branchCtrReg
	branchCond.io.less 	:= lessReg
	branchCond.io.zero 	:= zeroReg
	// Output
	val pcASrcReg 		= branchCond.io.pcASrc
	val pcBSrcReg 		= branchCond.io.pcBSrc
	
	/* Output */
	io.lsu2WBU.ready	:= 1.B
    io.wbu2CSR.pc       := pcReg
    io.wbu2CSR.csrWData := csrWDataReg
    io.wbu2CSR.csr      := instReg(31,20)
    io.wbu2CSR.ecall    := ecallReg
    io.wbu2CSR.csrEn    := csrEnReg
    io.wbu2CSR.csrWr    := csrWrReg
    io.wbu2BaseReg.data := MuxCase(	0.U(32.W), Seq(	
        (toRegReg === "b00".U).asBool -> aluDataReg,
		(toRegReg === "b01".U).asBool -> memDataReg,
		(toRegReg === "b10".U).asBool -> csrDataReg
    ))
    io.wbu2BaseReg.rdIndex  := instReg(11,7)
    io.wbu2BaseReg.regWR    := regWRReg
	io.wbu2PC.valid			:= handReg
    io.wbu2PC.bits.nextPC   := MuxCase(	0.U(32.W), Seq(	
        (pcASrcReg === "b00".U).asBool	-> 4.U,
		(pcASrcReg === "b01".U).asBool -> immDataReg,
		(pcASrcReg === "b10".U).asBool -> 0.U
    )) + MuxCase(	0.U(32.W), Seq(	
        (pcBSrcReg === "b00".U).asBool	-> pcReg,
		(pcBSrcReg === "b01".U).asBool -> rs1DataReg,
		(pcBSrcReg === "b10".U).asBool -> csrWDataReg
    ))

	io.wbu2Icache	:= fenceiReg
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
