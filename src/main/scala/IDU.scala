package singlecyclecpu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq

import singlecyclecpu._

class IDU(addrWidth: Int, dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val npcState = Input(UInt(5.W))
        val cmd      = Input(UInt(dataWidth.W))

        val insFormat = Output(UInt(32.W))
        val insType   = Output(UInt(3.W))
        // R-type
        val func7     = Output(UInt(7.W))
        val rs2       = Output(UInt(5.W))
        val rs1       = Output(UInt(5.W))
        val func3     = Output(UInt(3.W))
        val rd        = Output(UInt(5.W))
        val opcode    = Output(UInt(7.W))
        // I-type imm
        val iImm      = Output(UInt(12.W))
        // S-type imm
        val sImm      = Output(UInt(12.W))
        // B-type imm
        val bImm      = Output(UInt(13.W))
        // U-type imm
        val uImm      = Output(UInt(32.W))
        // J-type imm
        val jImm      = Output(UInt(21.W))
    })

    val func7Wire  = io.cmd(31, 25)
    val rs2Wire    = io.cmd(24, 20)
    val rs1Wire    = io.cmd(19, 15)
    val func3Wire  = io.cmd(14, 12)
    val rdWire     = io.cmd(11, 7)
    val opcodeWire = io.cmd(6, 0)
    val iImmWire   = io.cmd(31, 20)
    val sImmWire   = Cat(io.cmd(31, 25), io.cmd(11, 7))
    val bImmWire   = Cat(io.cmd(31), io.cmd(7), io.cmd(30, 25), io.cmd(11, 8), 0.U(1.W))
    val uImmWire   = Cat(io.cmd(31, 12), 0.U(12.W))
    val jImmWire   = Cat(io.cmd(31), io.cmd(19, 12), io.cmd(20), io.cmd(30, 21), 0.U(1.W))

    val insFormatReg = RegInit(0.U(32.W))
    val insTypeReg   = RegInit(InstructionType.NOP.asUInt)

    val cmdReg    = RegInit(0.U(dataWidth.W))
    val func7Reg  = RegInit(0.U(7.W))
    val rs2Reg    = RegInit(0.U(5.W))
    val rs1Reg    = RegInit(0.U(5.W))
    val func3Reg  = RegInit(0.U(3.W))
    val rdReg     = RegInit(0.U(5.W))
    val opcodeReg = RegInit(0.U(7.W))
    val iImmReg   = RegInit(0.U(12.W))
    val sImmReg   = RegInit(0.U(12.W))
    val bImmReg   = RegInit(0.U(13.W))
    val uImmReg   = RegInit(0.U(32.W))
    val jImmReg   = RegInit(0.U(21.W))

    val instructionFormatWire = MuxCase(InstructionFormat.NOP.asUInt, Seq(
		(func7Wire === "b0000000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U) -> InstructionFormat.ADD.asUInt,
		(func3Wire === "b000".U && opcodeWire === "b0010011".U) -> InstructionFormat.ADDI.asUInt
	))

    val instructionTypeWire = MuxCase(InstructionType.NOP.asUInt, Seq(
		(func7Wire === "b0000000".U && func3Wire === "b000".U && opcodeWire === "b0110011".U) -> InstructionType.R.asUInt,
		(func3Wire === "b000".U && opcodeWire === "b0010011".U) -> InstructionType.I.asUInt
	))

    when(io.npcState === NpcState.RUNNING.asUInt) {
		insFormatReg := instructionFormatWire
		insTypeReg   := instructionTypeWire
		cmdReg       := io.cmd
		func7Reg     := func7Wire
		rs2Reg       := rs2Wire
		rs1Reg       := rs1Wire
		func3Reg     := func3Wire
		rdReg        := rdWire
		opcodeReg    := opcodeWire
		iImmReg      := iImmWire
		sImmReg      := sImmWire
		bImmReg      := bImmWire
		uImmReg      := uImmWire
		jImmReg      := jImmWire
	}.elsewhen(io.npcState === NpcState.STOP.asUInt) {
		cmdReg    := cmdReg
		func7Reg  := func7Reg
		rs2Reg    := rs2Reg
		rs1Reg    := rs1Reg
		func3Reg  := func3Reg
		rdReg     := rdReg
		opcodeReg := opcodeReg
		iImmReg   := iImmReg
		sImmReg   := bImmReg
		uImmReg   := uImmReg
		jImmReg   := jImmReg
	}.otherwise{
		cmdReg    := 0.U(dataWidth.W)
		func7Reg  := 0.U(7.W)
		rs2Reg    := 0.U(5.W)
		rs1Reg    := 0.U(5.W)
		func3Reg  := 0.U(3.W)
		rdReg     := 0.U(5.W)
		opcodeReg := 0.U(7.W)
		iImmReg   := 0.U(12.W)
		sImmReg   := 0.U(12.W)
		bImmReg   := 0.U(13.W)
		uImmReg   := 0.U(32.W)
		jImmReg   := 0.U(21.W)
	}

    io.insFormat := insFormatReg.asUInt
    io.insType   := insTypeReg.asUInt
    io.func7     := func7Reg
    io.rs2       := rs2Reg
    io.rs1       := rs1Reg
    io.func3     := func3Reg
    io.rd        := rdReg
    io.opcode    := opcodeReg
    io.iImm      := iImmReg
    io.sImm      := sImmReg
    io.bImm      := bImmReg
    io.uImm      := uImmReg
    io.jImm      := jImmReg
}

