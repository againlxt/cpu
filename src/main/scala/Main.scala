package singlecyclecpu

import chisel3._
import chisel3.util._
import singlecyclecpu._

object Main extends App {
	emitVerilog(new TOP(), Array("--target-dir", "generated"))
}

class TOP() extends Module {
	val io = IO(new Bundle {
		val pc 		= Input(UInt(32.W))
		val memData = Input(UInt(32.W))

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

	val ifu = Module(new IFU(32, 32))
	val idu = Module(new IDU(32, 32))

	// IFU connection
	ifu.io.npcState := NpcState.RUNNING.asUInt
	ifu.io.pc 		:= io.pc
	ifu.io.memData 	:= io.memData
	val cmdWire 	= ifu.io.cmd
	val pcOutWire	= ifu.io.pcOut

	// IDU connection
	idu.io.npcState := NpcState.RUNNING.asUInt
	idu.io.cmd 		:= cmdWire

	val insFormatWire 	= idu.io.insFormat
	val insTypeWire 	= idu.io.insType
	val func7Wire 		= idu.io.func7
	val rs2Wire 		= idu.io.rs2
	val rs1Wire 		= idu.io.rs1
	val func3Wire 		= idu.io.func3
	val rdWire			= idu.io.rd
	val opcodeWire 		= idu.io.opcode
	val iImmWire		= idu.io.iImm
	val sImmWire		= idu.io.sImm
	val bImmWire 		= idu.io.bImm
	val uImmWire 		= idu.io.uImm
	val jImmWire 		= idu.io.jImm

	// Ouput
	io.insFormat 	:= insFormatWire
	io.insType 		:= insTypeWire
	io.func7 		:= func7Wire
	io.rs2 			:= rs2Wire
	io.rs1 			:= rs1Wire
	io.func3 		:= func3Wire
	io.rd 			:= rdWire
	io.opcode 		:= opcodeWire
	io.iImm 		:= iImmWire
	io.sImm 		:= sImmWire
	io.bImm 		:= bImmWire
	io.uImm 		:= uImmWire
	io.jImm 		:= jImmWire
}
