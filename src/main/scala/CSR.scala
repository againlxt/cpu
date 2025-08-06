package cpu

import chisel3._
import chisel3.util._
import scala.collection.immutable.ArraySeq
import _root_.interface.EXU2CSR
import _root_.interface.WBU2CSR

class CSRReg extends Module {
    val io = IO(new Bundle {
        val exu2CSR = Flipped(new EXU2CSR)
        val wbu2CSR = Flipped(new WBU2CSR)
    })

    // val csrReg 		= RegInit(VecInit(Seq.fill(4096)(0.U(32.W))))
    val mepcReg     	= RegInit(0.U(32.W))
    val mstatusReg  	= RegInit("h1800".U(32.W))
    val mcauseReg   	= RegInit(0.U(32.W))
    val mtvecReg    	= RegInit(0.U(32.W))
	val mvendoridReg 	= RegInit(0x79737978.U(32.W))
	val marchidReg 		= RegInit(0x78957352.U(32.W))
    when(io.wbu2CSR.csrEn.asBool) {
        when(io.wbu2CSR.csrWr === 1.U) {
            when(io.wbu2CSR.csr === "h300".U) {
                mstatusReg  	:= io.wbu2CSR.csrWData
            }.elsewhen(io.wbu2CSR.csr === "h342".U) {
                mcauseReg   	:= io.wbu2CSR.csrWData
            }.elsewhen(io.wbu2CSR.csr === "h341".U) {
                mepcReg     	:= io.wbu2CSR.csrWData
            }.elsewhen(io.wbu2CSR.csr === "h305".U) {
                mtvecReg    	:= io.wbu2CSR.csrWData
            }.elsewhen(io.wbu2CSR.csr === "hf11".U) {
				mvendoridReg 	:= io.wbu2CSR.csrWData
			}.elsewhen(io.wbu2CSR.csr === "hf12".U) {
				marchidReg 		:= io.wbu2CSR.csrWData
			}
        }.elsewhen(io.wbu2CSR.ecall === 1.B) {
            mepcReg     := io.wbu2CSR.pc
            mcauseReg   := 11.U
        }
    }

    io.exu2CSR.csrData := MuxCase(	0.U(32.W), Seq(	
        (io.exu2CSR.csr === "h300".U).asBool -> mstatusReg,
		(io.exu2CSR.csr === "h342".U).asBool -> mcauseReg,
		(io.exu2CSR.csr === "h341".U).asBool -> mepcReg,
		(io.exu2CSR.csr === "h305".U).asBool -> mtvecReg,
		(io.exu2CSR.csr === "hf11".U).asBool -> mvendoridReg,
		(io.exu2CSR.csr === "hf12".U).asBool -> marchidReg,
        io.exu2CSR.ecall.asBool              -> mtvecReg,
        io.exu2CSR.mret.asBool               -> mepcReg
    ))
    io.wbu2CSR.mtvec := mtvecReg
}
