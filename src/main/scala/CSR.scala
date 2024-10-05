package  singlecyclecpu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq

class CSRReg extends Module {
    val io = IO(new Bundle {
        val csr     = Input(UInt(12.W))
        val dataIn  = Input(UInt(32.W))
        val pc      = Input(UInt(32.W))
        val mret    = Input(Bool())
        val ecall   = Input(Bool())
        val csrEn   = Input(Bool())
        val csrWr   = Input(Bool())
        val csrData = Output(UInt(32.W))
    })

    // val csrReg 	= RegInit(VecInit(Seq.fill(4096)(0.U(32.W))))
    val mepcReg     = RegInit(0.U(32.W))
    val mstatusReg  = RegInit("h1800".U(32.W))
    val mcauseReg   = RegInit(0.U(32.W))
    val mtvecReg    = RegInit(0.U(32.W))
    when(io.csrEn) {
        when(io.csrWr === 1.U) {
            when(io.csr === "h300".U) {
                mstatusReg  := io.dataIn
            }.elsewhen(io.csr === "h342".U) {
                mcauseReg   := io.dataIn
            }.elsewhen(io.csr === "h341".U) {
                mepcReg     := io.dataIn
            }.elsewhen(io.csr === "h305".U) {
                mtvecReg    := io.dataIn
            }
        }.elsewhen(io.ecall === 1.B) {
            mepcReg     := io.pc
            mcauseReg   := 11.U
        }
    }

    io.csrData := MuxCase(	0.U(32.W), Seq(	
        (io.csr === "h300".U).asBool -> mstatusReg,
		(io.csr === "h342".U).asBool -> mcauseReg,
		(io.csr === "h341".U).asBool -> mepcReg,
		(io.csr === "h305".U).asBool -> mtvecReg,
        io.ecall                     -> mtvecReg,
        io.mret                      -> mepcReg
    ))
}
