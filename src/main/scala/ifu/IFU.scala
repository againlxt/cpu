package ifu

import chisel3._
import chisel3.util._
import interface._
//import singlecyclecpu._

class IFU extends Module {
    val io = IO(new Bundle {
        val memData  = Input(new CBIFU)
        val inst     = Output(new IFUBIDU)
    })

    val pcWire      = io.pc
    val memDataWire = io.memData

    io.cmd  := memDataWire
}
