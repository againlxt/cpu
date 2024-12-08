package device

import chisel3._
import chisel3.util._
import _root_.interface.AXILite
import memory.AXILiteBusArbiter
import memory.AXILiteSram

object DeviceID extends ChiselEnum{
	val UART, CLINT, INIT = Value
}

object DeviceUart {
	val baseAddr	= "hA00003F8".U
}

object DeviceClint {
	val baseAddr 	= "hA0000048".U
	val size 		= 8.U
}

class Xbar extends Module {
	val io = IO(new Bundle {
		val axiLiteMaster0 	= Flipped(new AXILite)
		val axiLiteMaster1 	= Flipped(new AXILite)
		val axiLiteSram    	= new AXILite 
		val axiLiteUart    	= new AXILite
		val axiLiteClint	= new AXILite
	})
	def initializeAXILite(axiLite: AXILite): Unit = {
		axiLite.arAddr := 0.U
		axiLite.arValid := false.B
		axiLite.rReady := false.B
		axiLite.awAddr := 0.U
		axiLite.awValid := false.B
		axiLite.wData := 0.U
		axiLite.wStrb := 0.U
		axiLite.wValid := false.B
		axiLite.bReady := false.B
	}
	initializeAXILite(io.axiLiteSram)
  	initializeAXILite(io.axiLiteUart)
	initializeAXILite(io.axiLiteClint)

	val axiLiteBusArbiter 	= Module(new AXILiteBusArbiter)
	io.axiLiteMaster0 <> axiLiteBusArbiter.io.axiLiteMaster0
	io.axiLiteMaster1 <> axiLiteBusArbiter.io.axiLiteMaster1
	val axiLiteSlave = axiLiteBusArbiter.io.axiLiteSlave

	val deviceID = MuxCase(DeviceID.INIT, Seq(
		(axiLiteSlave.arAddr === DeviceUart.baseAddr) -> DeviceID.UART,
		(DeviceClint.baseAddr === axiLiteSlave.arAddr || axiLiteSlave.arAddr === DeviceClint.baseAddr + DeviceClint.size - 4.U) -> DeviceID.CLINT
	))
	val skipDiff = Module(new SkipDiff())
    skipDiff.io.en := (deviceID =/= DeviceID.INIT)
	when(deviceID === DeviceID.UART) {
		axiLiteSlave <> io.axiLiteUart
	} .elsewhen(deviceID === DeviceID.CLINT) {
		axiLiteSlave <> io.axiLiteClint
	}.otherwise {
		axiLiteSlave <> io.axiLiteSram
	}	
}

class AXILiteUart extends Module {
    val io = IO(new Bundle {
        val axiLiteMaster    = Flipped(new AXILite)
    })
	val uart = Module(new UartV);
	uart.io.en := 0.B
	uart.io.data := 0.U

    val aresetnWire          = 1.B - this.reset.asBool
    io.axiLiteMaster.arReady := 1.B
    io.axiLiteMaster.rData   := 0.U(32.W)
    io.axiLiteMaster.rrEsp   := 0.U(2.W)
    io.axiLiteMaster.rValid  := 0.B
    io.axiLiteMaster.awReady := 1.B
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

    val awEnReg             = RegInit(1.B)
    val uartEnReg           = RegInit(0.B)
    val uartDataReg         = RegInit(8.U)
    uart.io.data            := uartDataReg
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
        uartEnReg   := 0.B
    }
    when(~aresetnWire.asBool) {
        awAddrReg   := 0.U
    } .elsewhen(~awReadyReg && awValidWire && wValidWire && awEnReg) {
        awAddrReg   := awAddrWire
    }
    /* W */
	uart.io.en := uartEnReg
    when(~aresetnWire.asBool) {
        wReadyReg   := 0.B
        uartDataReg := 0.B
    } .elsewhen(~wReadyReg && wValidWire && awValidWire && awEnReg) {
        wReadyReg   := 1.B
		uartDataReg := wDataWire
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

	setInline("UartV.sv",
	"""module UartV(
	|	input en,
	|	input [7:0]	data
	|);
	|
	|import "DPI-C" function void uart(input byte chr);
	|always@(en) begin
	|	if(en) uart(data);
	|end
	|
	|endmodule
	""".stripMargin)
}

class SkipDiff extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val en = Input(Bool())
    })

	setInline("SkipDiff.sv",
	"""module SkipDiff(
	|	input en
	|);
	|
	|import "DPI-C" function void difftest_skip_ref();
	|always@(en) begin
	|	if(en) difftest_skip_ref();
	|end
	|
	|endmodule
	""".stripMargin)
}

class AXILiteClint extends Module {
	val io = IO(new Bundle {
        val axiLiteMaster    = Flipped(new AXILite)
    })

	val mtimeReg = RegInit(0.U(64.W))
	when(this.clock.asBool) {
		mtimeReg := mtimeReg + 1.U;
	}

    val aresetnWire          = 1.B - this.reset.asBool
    io.axiLiteMaster.arReady := 1.B
    io.axiLiteMaster.rData   := 0.U(32.W)
    io.axiLiteMaster.rrEsp   := 0.U(2.W)
    io.axiLiteMaster.rValid  := 0.B
    io.axiLiteMaster.awReady := 1.B
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

    val awEnReg             = RegInit(1.B)
    val awAddrReg           = RegInit(0.U(32.W))
    val arAddrReg           = RegInit(0.U(32.W))

    /* AXI Transport */
    /* AW */
    when(~aresetnWire.asBool) {
        awReadyReg  := 1.B
        awEnReg     := 1.B
    } .elsewhen(awValidWire && wValidWire && ~awReadyReg && awEnReg) {
        awReadyReg  := 1.B
        awEnReg     := 0.B
    } .elsewhen(wValidWire && wReadyReg) {
        awReadyReg  := 0.B
        awEnReg     := 1.B
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
		when(arAddrReg === DeviceClint.baseAddr) {
			rDataReg:= mtimeReg(31,0)
		} .elsewhen(arAddrReg === DeviceClint.baseAddr + 4.U) {
			rDataReg:= mtimeReg(63,32)
		} .otherwise {
			rDataReg:= 0.U
		}
        bRespReg    := 0.U
    }
}
