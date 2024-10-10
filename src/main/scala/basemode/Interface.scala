package interface

import chisel3._
import chisel3.util._
import chisel3.util.Decoupled

/* Bus */
class IFUBIDU extends Bundle {
    val inst = Output(UInt(32.W))
}

/* Normal */
class CBIFU extends Bundle {
    val memData = Input(UInt(32.W))
}
