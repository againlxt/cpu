package device

import chisel3._
import chisel3.util._
import _root_.interface.AXILite

class AXILiteUart extends Module {
    val io = IO(new Bundle {
        val axiLiteMaster    = Flipped(new AXILite)
    })
    val aresetnWire          = 1.B - this.reset.asBool
    io.axiLiteMaster.arReady := 0.B
    io.axiLiteMaster.rData   := 0.U(32.W)
    io.axiLiteMaster.rrEsp   := 0.U(2.W)
    io.axiLiteMaster.rValid  := 0.B
    io.axiLiteMaster.awReady := 0.B
    io.axiLiteMaster.wReady  := 0.B
    io.axiLiteMaster.bResp   := 0.U(2.W)
    io.axiLiteMaster.bValid  := 0.B

    /* AR */
    val arAddrWire          = io.axiLiteMaster.arAddr
    val arValidWire         = io.axiLiteMaster.arValid
    val arReadyReg          = RegInit(1.B)
    io.axiLiteMaster.arReady:= arReadyReg
    val arReadyWire         = io.axiLiteMaster.arReady
    /* R */
    val rDataReg            = RegInit(0.U(32.W))
    io.axiLiteMaster.rData  := rDataReg
    val rrEspReg            = RegInit(0.U(2.W))
    io.axiLiteMaster.rrEsp  := rrEspReg
    val rValidReg           = RegInit(0.B)
    io.axiLiteMaster.rValid := rValidReg
    val rReadyWire          = io.axiLiteMaster.rReady
    /* AW */
    val awAddrWire          = io.axiLiteMaster.awAddr
    val awValidWire         = io.axiLiteMaster.awValid
    val awReadyReg          = RegInit(1.B)
    io.axiLiteMaster.awReady:= awReadyReg
    /* W */
    val wDataWire           = io.axiLiteMaster.wData
    val wStrbWire           = io.axiLiteMaster.wStrb
    val wValidWire          = io.axiLiteMaster.wValid
    val wReadyReg           = RegInit(0.B)
    io.axiLiteMaster.wReady := wReadyReg
    /* B */
    val bRespReg            = RegInit(0.U(2.W))
    io.axiLiteMaster.bResp  := bRespReg
    val bValidReg           = RegInit(0.B)
    io.axiLiteMaster.bValid := bValidReg
    val bReadyWire          = io.axiLiteMaster.bReady

    val awEnReg             = RegInit(0.B)
    val uartEnReg           = RegInit(0.B)
    val awAddrReg           = RegInit(0.U(32.W))
    val arAddrReg           = RegInit(0.U(32.W))

    /* AXI Transport */
    /* AW */
    when(~aresetnWire.asBool) {
        awReadyReg  := 1.B
        awEnReg     := 1.B
        uartEnReg   := 0.B
    } .elsewhen(awValidWire && wValidWire && ~awReadyReg && awEnReg) {
        awReadyReg  := 1.B
        awEnReg     := 0.B
        uartEnReg   := 0.B
    } .elsewhen(wValidWire && wReadyReg) {
        awReadyReg  := 0.B
        awEnReg     := 1.B
        uartEnReg   := 1.B
    } .otherwise {
        awReadyReg  := 0.B
    }
    when(~aresetnWire.asBool) {
        awAddrReg   := 0.U
    } .elsewhen(~awReadyReg && awValidWire && wValidWire && awEnReg) {
        awAddrReg   := awAddrWire
    }
    /* W */
    when(~aresetnWire.asBool) {
        wReadyReg   := 0.B
    } .elsewhen(~wReadyReg && wValidWire && awValidWire && awEnReg) {
        wReadyReg   := 1.B
    } .otherwise {
        wReadyReg   := 0.B
    }
    /* B */
    when(~aresetnWire.asBool) {
        bValidReg   := 0.B
    } .elsewhen(awValidWire && awReadyReg && wValidWire && wReadyReg && ~bValidReg) {
        bValidReg   := 1.B
    } .elsewhen(bValidReg && bReadyWire) {
        bValidReg   := 0.B
    }
    /* AR */
    when(~aresetnWire.asBool) {
        arReadyReg  := 1.B
        arAddrReg   := 0.U
    } .elsewhen(arValidWire && ~arReadyReg) {
        arReadyReg  := 1.B
        arAddrReg   := arAddrWire
    } .otherwise {
        arReadyReg  := 0.B
    }
    /* R */
    when(~aresetnWire.asBool) {
        rValidReg   := 0.B
        rrEspReg    := 0.U
    } .elsewhen(arValidWire && arReadyReg && ~rValidReg) {
        rValidReg   := 1.B
        rrEspReg    := 0.U
    } .elsewhen(rReadyWire) {
        rValidReg   := 0.B
    }
    when(~aresetnWire.asBool) {
        rDataReg    := 0.U
        bRespReg    := 0.U
    } .elsewhen(arValidWire && arReadyReg && ~rValidReg) {
        bRespReg    := 0.U
    }
}

class UartV extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val en      = Input(Bool())
        val data    = Input(UInt(8.W))
    })
}
