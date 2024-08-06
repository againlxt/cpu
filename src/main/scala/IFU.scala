package singlecyclecpu

import chisel3._
import chisel3.util._
import singlecyclecpu._

class IFU(addrWidth: Int, dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val npcState = Input(UInt(5.W))
        val pc       = Input(UInt(addrWidth.W))
        val memData  = Input(UInt(dataWidth.W))
        val cmd      = Output(UInt(dataWidth.W))
        val pcOut    = Output(UInt(addrWidth.W))
    })

    val pcReg      = RegInit(0.U(addrWidth.W))
    val memDataReg = RegInit(0.U(dataWidth.W))
    val pcOutReg   = RegInit(0.U(addrWidth.W))

    when(io.npcState === NpcState.RUNNING.asUInt) {
        pcReg      := io.pc
        memDataReg := io.memData
        pcOutReg   := io.pc
    }.elsewhen(io.npcState === NpcState.STOP.asUInt) {
        pcReg      := pcReg
        memDataReg := memDataReg
        pcOutReg   := pcOutReg
    }.otherwise{
        pcReg      := 0.U(addrWidth.W)
        memDataReg := 0.U(dataWidth.W)
        pcOutReg   := 0.U(addrWidth.W)
    }

    io.cmd  := memDataReg
    io.pcOut := pcOutReg
}
