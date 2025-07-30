package idu

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import scala.collection.immutable.ArraySeq
import interface._
import cpu.Config

import idu._
import _root_.interface._
import dpic._

class IDU extends Module {
    val io = IO(new Bundle{
		val inst 			= Flipped(Decoupled(new IFU2IDU))
		val idu2EXU			= Decoupled(new IDU2EXU)
		val idu2BaseReg 	= new IDU2BaseReg
	})

    val pcReg       = RegNext(io.inst.bits.pc)
    val instReg     = RegNext(io.inst.bits.inst)
    io.inst.ready   := 1.B
    val valid2EXUReg= RegNext(io.inst.ready & io.inst.valid)
    io.idu2EXU.valid:= valid2EXUReg
    
    val instWire    = instReg
    val pcWire      = pcReg

    val func7Wire  	= instWire(31, 25)
    val rs2IndexWire= instWire(24, 20)
    val rs1IndexWire= instWire(19, 15)
    val func3Wire  	= instWire(14, 12)
    val rdIndexWire = instWire(11, 7)
    val opcodeWire 	= instWire(6, 0)
    val iImmWire   	= instWire(31, 20)
    val sImmWire   	= Cat(instWire(31, 25), instWire(11, 7))
    val bImmWire   	= Cat(instWire(31), instWire(7), instWire(30, 25), instWire(11, 8), 0.U(1.W))
    val uImmWire   	= Cat(instWire(31, 12), 0.U(12.W))
    val jImmWire   	= Cat(instWire(31), instWire(19, 12), instWire(20), instWire(30, 21), 0.U(1.W))

	// Instantitate ContrGen
    val contrGen 	= Module(new ContrGen())
	// Input
    contrGen.io.cmd 	:= instWire
    contrGen.io.opcode  := opcodeWire
    contrGen.io.func3 	:= func3Wire
    contrGen.io.func7 	:= func7Wire
	// Output
    val immTypewire	= contrGen.io.immType
    val regWRWire 	= contrGen.io.regWR
    val srcAALUWire = contrGen.io.srcAALU
    val srcBALUWire = contrGen.io.srcBALU
    val ctrALUWire 	= contrGen.io.ctrALU
    val branchWire 	= contrGen.io.branch
    val memToRegWire= contrGen.io.memToReg
    val memWRWire 	= contrGen.io.memWR
    val memValidWire= contrGen.io.memValid
    val memOPWire 	= contrGen.io.memOP
    val ecallWire   = contrGen.io.ecall
    val mretWire    = contrGen.io.mret
    val csrEnWire   = contrGen.io.csrEn
    val csrWrWire   = contrGen.io.csrWr
    val csrOPWire   = contrGen.io.csrOP
    val csrALUOPWire= contrGen.io.csrALUOP

	// Instantitate ImmGen
    val immGen 		= Module(new ImmGen)
	// Input
    immGen.io.iImm 	:= iImmWire
    immGen.io.sImm 	:= sImmWire
    immGen.io.bImm 	:= bImmWire
    immGen.io.uImm 	:= uImmWire
    immGen.io.jImm 	:= jImmWire
    immGen.io.immType 	:= immTypewire
	// Output
    val immWire 	= immGen.io.imm

    /* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
        val instType = MuxCase(PerformanceCounterType.OTHER.asUInt, Seq(
            (opcodeWire === "b1100111".U || opcodeWire === "b1101111".U).asBool -> PerformanceCounterType.JUMP.asUInt,
            (opcodeWire === "b0100011".U).asBool                                -> PerformanceCounterType.STROE.asUInt,
            (opcodeWire === "b0000011".U).asBool                                -> PerformanceCounterType.LOAD.asUInt,
            (opcodeWire === "b0010011".U || opcodeWire === "b0110011".U).asBool -> PerformanceCounterType.CAL.asUInt,
            (opcodeWire === "b1110011".U).asBool                                -> PerformanceCounterType.CSR.asUInt
        ))
        val instTypeCnt             = Module(new PerformanceCounter)
        instTypeCnt.io.valid        := io.idu2EXU.valid
        instTypeCnt.io.counterType  := instType
        instTypeCnt.io.data         := 0.U
	}

	// Output
    io.idu2EXU.bits.regWR 	 	:= regWRWire
    io.idu2EXU.bits.srcAALU 	:= srcAALUWire
    io.idu2EXU.bits.srcBALU 	:= srcBALUWire
    io.idu2EXU.bits.ctrALU 		:= ctrALUWire
    io.idu2EXU.bits.branch 		:= branchWire
    io.idu2EXU.bits.toReg 		:= memToRegWire
    io.idu2EXU.bits.memWR 		:= memWRWire
	io.idu2EXU.bits.memValid 	:= memValidWire 
    io.idu2EXU.bits.memOP 		:= memOPWire
    io.idu2EXU.bits.rs1Index    := rs1IndexWire
    io.idu2EXU.bits.ecall       := ecallWire
    io.idu2EXU.bits.mret        := mretWire
    io.idu2EXU.bits.csrEn       := csrEnWire
    io.idu2EXU.bits.csrWr       := csrWrWire
    io.idu2EXU.bits.csrOP       := csrOPWire
    io.idu2EXU.bits.csrALUOP    := csrALUOPWire

    io.idu2BaseReg.rs1Index := rs1IndexWire
    io.idu2BaseReg.rs2Index := rs2IndexWire

    io.idu2EXU.bits.pc          := pcWire
    io.idu2EXU.bits.rs1Data 	:= io.idu2BaseReg.rs1Data
    io.idu2EXU.bits.rs2Data 	:= io.idu2BaseReg.rs2Data
    io.idu2EXU.bits.imm 		:= immWire
    io.idu2EXU.bits.inst        := instWire
}
