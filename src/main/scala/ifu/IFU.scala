package ifu

import chisel3._
import chisel3.util._
import singlecyclecpu._

class IFU extends Module {
    val io = IO(new Bundle {
        val pc       = Input(UInt(32.W))
        val memData  = Input(UInt(32.W))
        val cmd      = Output(UInt(32.W))
    })

    val pcWire      = io.pc
    val memDataWire = io.memData

    io.cmd  := memDataWire
}
