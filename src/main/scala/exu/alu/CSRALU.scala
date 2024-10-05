package alu

import chisel3._
import chisel3.util._
import baseexu._
import alu._

class CSRALU extends Module {
	val io = IO(new Bundle {
		val srcAData 	= Input(UInt(32.W))
		val srcBData 	= Input(UInt(32.W))
		val csrALUOP    = Input(UInt(2.W))

        val oData       = Output(UInt(32.W))
    })
    val srcADataWire    = io.srcAData
    val srcBDataWire    = io.srcBData
    val csrALUOPWire    = io.csrALUOP

    val andNotDataWire  = srcADataWire & (~srcBDataWire)
    val orDataWire      = srcADataWire | srcBDataWire
    
    io.oData := MuxCase(0.U, Seq(
        (csrALUOPWire === 0.U).asBool -> andNotDataWire,
        (csrALUOPWire === 1.U).asBool -> orDataWire,
        (csrALUOPWire === 2.U).asBool -> srcBDataWire
    ))
}