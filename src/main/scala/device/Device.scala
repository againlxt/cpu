package device

import chisel3._
import chisel3.util._
import _root_.interface._
import memory._
import cpu.Config 
import _root_.interface.AXIUtils.initializeAXIMaster

object DeviceID extends ChiselEnum{
	val CLINT, UART16550, MSPI, GPIO, PS2, VGA, SRAM, MROM, OTHER, ERROR = Value
}

object DeviceClint {
	val baseAddr 	= 0x02000000.U
	val size 		= 0x10000.U
}

object DeviceUart16550 {
	val baseAddr	= 0x10000000.U
	val size 		= 0x1000.U
}

// object DeviceClint {
//     val baseAddr 	= BigInt("A0000048", 16).U(32.W)
//     val size 		= 8.U
// }

// object DeviceUart16550 {
//     val baseAddr	= BigInt("A00003F8", 16).U(32.W)
//     val size 		= 1.U
// }

object DeviceMSPI {
	val baseAddr	= 0x10001000.U
	val size 		= 0x1000.U
}

object DeviceGPIO {
	val baseAddr	= 0x10002000.U
	val size 		= 0x8.U
}

object DevicePS2 {
	val baseAddr	= 0x10011000.U
	val size 		= 0x8.U
}

object DeviceVGA {
	val baseAddr	= 0x21000000.U
	val size 		= 0x00200000.U
}

object DeviceSRAM {
	val baseAddr	= 0x0F000000.U
	val size 		= 0x00002000.U
}

object DeviceMROM {
	val baseAddr	= 0x20000000.U
	val size 		= 0x00001000.U
}

object DeviceOTHER {
	val baseAddr	= 0x30000000.U
}

class XbarAXI extends Module {
    val io = IO(new Bundle {
        val axiSlaveIFU     = Flipped(new AXI)
        val axiSlaveLSU     = Flipped(new AXI)
        val axiMasterDevice = new AXI
		val axiLiteClint	= new AXILite
        val axiLiteUart     = if (!Config.SoC) Some(new AXILite) else None
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
    
    AXIUtils.initializeAXISlave(io.axiSlaveIFU)
    AXIUtils.initializeAXISlave((io.axiSlaveLSU))
    AXIUtils.initializeAXIMaster(io.axiMasterDevice)

	initializeAXILite(io.axiLiteClint)
    if(!Config.SoC) io.axiLiteUart.foreach(uart => initializeAXILite(uart))

    val axiBusarbiter   = Module(new AXIBusArbiter)
    AXIUtils.initializeAXIMaster(axiBusarbiter.io.axiSlave1)
    io.axiSlaveIFU      <> axiBusarbiter.io.axiSlave0
    io.axiMasterDevice  <> axiBusarbiter.io.axiMaster

    val deviceID = MuxCase(DeviceID.ERROR, Seq(
		(((io.axiSlaveLSU.araddr < DeviceClint.baseAddr + DeviceClint.size) & (io.axiSlaveLSU.araddr >= DeviceClint.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DeviceClint.baseAddr + DeviceClint.size) & (io.axiSlaveLSU.awaddr >= DeviceClint.baseAddr))) -> DeviceID.CLINT,
		(((io.axiSlaveLSU.araddr < DeviceUart16550.baseAddr + DeviceUart16550.size) & (io.axiSlaveLSU.araddr >= DeviceUart16550.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DeviceUart16550.baseAddr + DeviceUart16550.size) & (io.axiSlaveLSU.awaddr >= DeviceUart16550.baseAddr))) -> DeviceID.UART16550,
		(((io.axiSlaveLSU.araddr < DeviceMSPI.baseAddr + DeviceMSPI.size) & (io.axiSlaveLSU.araddr >= DeviceMSPI.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DeviceMSPI.baseAddr + DeviceMSPI.size) & (io.axiSlaveLSU.awaddr >= DeviceMSPI.baseAddr))) -> DeviceID.MSPI,
		(((io.axiSlaveLSU.araddr < DeviceGPIO.baseAddr + DeviceGPIO.size) & (io.axiSlaveLSU.araddr >= DeviceGPIO.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DeviceGPIO.baseAddr + DeviceGPIO.size) & (io.axiSlaveLSU.awaddr >= DeviceGPIO.baseAddr))) -> DeviceID.GPIO,
		(((io.axiSlaveLSU.araddr < DevicePS2.baseAddr + DevicePS2.size) & (io.axiSlaveLSU.araddr >= DevicePS2.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DevicePS2.baseAddr + DevicePS2.size) & (io.axiSlaveLSU.awaddr >= DevicePS2.baseAddr))) -> DeviceID.PS2,
		(((io.axiSlaveLSU.araddr < DeviceVGA.baseAddr + DeviceVGA.size) & (io.axiSlaveLSU.araddr >= DeviceVGA.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DeviceVGA.baseAddr + DeviceVGA.size) & (io.axiSlaveLSU.awaddr >= DeviceVGA.baseAddr))) -> DeviceID.VGA,
        (((io.axiSlaveLSU.araddr < DeviceSRAM.baseAddr + DeviceSRAM.size) & (io.axiSlaveLSU.araddr >= DeviceSRAM.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DeviceSRAM.baseAddr + DeviceSRAM.size) & (io.axiSlaveLSU.awaddr >= DeviceSRAM.baseAddr))) -> DeviceID.SRAM,
        (((io.axiSlaveLSU.araddr < DeviceMROM.baseAddr + DeviceMROM.size) & (io.axiSlaveLSU.araddr >= DeviceMROM.baseAddr)) | 
		((io.axiSlaveLSU.awaddr < DeviceMROM.baseAddr + DeviceMROM.size) & (io.axiSlaveLSU.awaddr >= DeviceMROM.baseAddr))) -> DeviceID.MROM,
        ((io.axiSlaveLSU.araddr >= DeviceOTHER.baseAddr) | (io.axiSlaveLSU.awaddr >= DeviceOTHER.baseAddr)) -> DeviceID.OTHER
    ))

    assert(deviceID != DeviceID.ERROR);

    when(deviceID === DeviceID.CLINT) {
        /* AW */
        io.axiSlaveLSU.awready    := io.axiLiteClint.awReady
        io.axiLiteClint.awValid      := io.axiSlaveLSU.awvalid
        io.axiLiteClint.awAddr       := io.axiSlaveLSU.awaddr
        /* W */
        io.axiSlaveLSU.wready     := io.axiLiteClint.wReady
        io.axiLiteClint.wValid       := io.axiSlaveLSU.wvalid
        io.axiLiteClint.wData        := io.axiSlaveLSU.wdata
        io.axiLiteClint.wStrb        := io.axiSlaveLSU.wstrb
        /* B */
        io.axiSlaveLSU.bresp      := io.axiLiteClint.bResp
        io.axiSlaveLSU.bvalid     := io.axiLiteClint.bValid
        io.axiLiteClint.bReady       := io.axiSlaveLSU.bready
        /* AR */
        io.axiSlaveLSU.arready    := io.axiLiteClint.arReady
        io.axiLiteClint.arValid      := io.axiSlaveLSU.arvalid
        io.axiLiteClint.arAddr       := io.axiSlaveLSU.araddr
        /* R */
        io.axiSlaveLSU.rdata      := io.axiLiteClint.rData
        io.axiSlaveLSU.rresp      := io.axiLiteClint.rrEsp
        io.axiSlaveLSU.rvalid     := io.axiLiteClint.rValid
        io.axiSlaveLSU.rlast     := 1.B
        io.axiLiteClint.rReady       := io.axiSlaveLSU.rready
	} .elsewhen(deviceID === DeviceID.UART16550 && 
    ((io.axiSlaveLSU.araddr === BigInt("a00003f8", 16).U(32.W)) ||
    (io.axiSlaveLSU.awaddr === BigInt("a00003f8", 16).U(32.W)))) {
        io.axiLiteUart.foreach { axiLite =>
            /* AW */
            io.axiSlaveLSU.awready := axiLite.awReady
            axiLite.awValid   := io.axiSlaveLSU.awvalid
            axiLite.awAddr    := io.axiSlaveLSU.awaddr

            /* W */
            io.axiSlaveLSU.wready := axiLite.wReady
            axiLite.wValid    := io.axiSlaveLSU.wvalid
            axiLite.wData     := io.axiSlaveLSU.wdata
            axiLite.wStrb     := io.axiSlaveLSU.wstrb

            /* B */
            io.axiSlaveLSU.bresp := axiLite.bResp
            io.axiSlaveLSU.bvalid := axiLite.bValid
            axiLite.bReady    := io.axiSlaveLSU.bready

            /* AR */
            io.axiSlaveLSU.arready := axiLite.arReady
            axiLite.arValid   := io.axiSlaveLSU.arvalid
            axiLite.arAddr    := io.axiSlaveLSU.araddr

            /* R */
            io.axiSlaveLSU.rdata := axiLite.rData
            io.axiSlaveLSU.rresp := axiLite.rrEsp
            io.axiSlaveLSU.rvalid := axiLite.rValid
            io.axiSlaveLSU.rlast := 1.B
            axiLite.rReady    := io.axiSlaveLSU.rready
        }
 
    } .otherwise {
        io.axiSlaveLSU <> axiBusarbiter.io.axiSlave1
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
