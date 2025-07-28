package memory
import chisel3._
import chisel3.util._
import _root_.interface._
import basemode.Delay
import basemode.LFSR
import chisel3.experimental.IntParam

class ReadWriteSmem(dataWidth: Int, addrWidth: Int, memorySize: Int) extends Module {
  val io = IO(new Bundle {
    val enable  = Input(Bool())
    val write   = Input(Bool())
    val addr    = Input(UInt(addrWidth.W))
    val len     = Input(UInt(3.W))            // len 可以是 1, 2, 或 4
    val dataIn  = Input(UInt(dataWidth.W))
    val dataOut = Output(UInt(dataWidth.W))
  })

  // 使用8位宽度的内存
  val mem = SyncReadMem(memorySize, UInt(8.W))

  // 初始化输出数据
  io.dataOut := 0.U

  when(io.enable) {
    when(io.write) {
      // 写操作：根据 io.len 确定写入字节数
      when(io.len === 1.U) {
        mem.write(io.addr, io.dataIn(7, 0))
      } .elsewhen(io.len === 2.U) {
        mem.write(io.addr, io.dataIn(7, 0))
        mem.write(io.addr + 1.U, io.dataIn(15, 8))
      } .elsewhen(io.len === 4.U) {
        mem.write(io.addr, io.dataIn(7, 0))
        mem.write(io.addr + 1.U, io.dataIn(15, 8))
        mem.write(io.addr + 2.U, io.dataIn(23, 16))
        mem.write(io.addr + 3.U, io.dataIn(31, 24))
      } .otherwise{}
    } .otherwise {
      // 读操作：根据 io.len 确定读取字节数
      when(io.len === 1.U) {
        io.dataOut := mem.read(io.addr, io.enable)
      } .elsewhen(io.len === 2.U) {
        io.dataOut := Cat(mem.read(io.addr + 1.U, io.enable), mem.read(io.addr, io.enable))
      } .elsewhen(io.len === 4.U) {
        io.dataOut := Cat(
          mem.read(io.addr + 3.U, io.enable),
          mem.read(io.addr + 2.U, io.enable),
          mem.read(io.addr + 1.U, io.enable),
          mem.read(io.addr, io.enable)
        )
      } .otherwise{}
    }
  } .otherwise {}
}

class AXILiteBusArbiter extends Module {
  val io = IO(new Bundle {
    val axiLiteMaster0 = Flipped(new AXILite)
    val axiLiteMaster1 = Flipped(new AXILite)
    val axiLiteSlave  = new AXILite
  })
  io.axiLiteMaster0.arReady := 0.B
  io.axiLiteMaster0.rData   := 0.U(32.W)
  io.axiLiteMaster0.rrEsp   := 0.U(2.W)
  io.axiLiteMaster0.rValid  := 0.B
  io.axiLiteMaster0.awReady := 0.B
  io.axiLiteMaster0.wReady  := 0.B
  io.axiLiteMaster0.bResp   := 0.U(2.W)
  io.axiLiteMaster0.bValid  := 0.B

  io.axiLiteMaster1.arReady := 0.B
  io.axiLiteMaster1.rData   := 0.U(32.W)
  io.axiLiteMaster1.rrEsp   := 0.U(2.W)
  io.axiLiteMaster1.rValid  := 0.B
  io.axiLiteMaster1.awReady := 0.B
  io.axiLiteMaster1.wReady  := 0.B
  io.axiLiteMaster1.bResp   := 0.U(2.W)
  io.axiLiteMaster1.bValid  := 0.B

  io.axiLiteSlave.arAddr    := 0.U(32.W)
  io.axiLiteSlave.arValid   := 0.B
  io.axiLiteSlave.rReady    := 0.B
  io.axiLiteSlave.awAddr    := 0.U(32.W)
  io.axiLiteSlave.awValid   := 0.B
  io.axiLiteSlave.wData     := 0.U(32.W)
  io.axiLiteSlave.wStrb     := 0.U(4.W)
  io.axiLiteSlave.wValid    := 0.B
  io.axiLiteSlave.bReady    := 0.B

  val arValidWire0  = io.axiLiteMaster0.arValid
  val arReadyWire0  = io.axiLiteMaster0.arReady
  val awValidWire0  = io.axiLiteMaster0.awValid
  val awReadyWire0  = io.axiLiteMaster0.awReady
  val wValidWire0   = io.axiLiteMaster0.wValid
  val wReadyWire0   = io.axiLiteMaster0.wReady
  val rValidWire0   = io.axiLiteMaster0.rValid
  val rReadyWire0   = io.axiLiteMaster0.rReady
  val bValidWire0   = io.axiLiteMaster0.bValid
  val bReadyWire0   = io.axiLiteMaster0.bReady

  val arValidWire1  = io.axiLiteMaster1.arValid
  val arReadyWire1  = io.axiLiteMaster1.arReady
  val awValidWire1  = io.axiLiteMaster1.awValid
  val awReadyWire1  = io.axiLiteMaster1.awReady
  val wValidWire1   = io.axiLiteMaster1.wValid
  val wReadyWire1   = io.axiLiteMaster1.wReady
  val rValidWire1   = io.axiLiteMaster1.rValid
  val rReadyWire1   = io.axiLiteMaster1.rReady
  val bValidWire1   = io.axiLiteMaster1.bValid
  val bReadyWire1   = io.axiLiteMaster1.bReady

  val s_idle :: s_wait :: s_ifu :: s_lsu :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val wait2LSUWire  = arValidWire1.asBool || awValidWire1.asBool || wValidWire1.asBool
  val wait2IFUWire  = arValidWire0.asBool || awValidWire0.asBool || wValidWire0.asBool
  val ifu2WaitWire  = (rValidWire0.asBool && rReadyWire0.asBool) || (wValidWire0.asBool && wReadyWire0.asBool)
  val lsu2WaitWire  = (rValidWire1.asBool && rReadyWire1.asBool) || (wValidWire1.asBool && wReadyWire1.asBool)
  state := MuxLookup(state, s_idle)(List(
    s_idle  -> Mux(this.reset.asBool, s_idle, s_wait),
    s_wait  -> Mux(this.reset.asBool, s_idle, Mux(wait2LSUWire, s_lsu, Mux(wait2IFUWire, s_ifu, s_wait))),
    s_ifu   -> Mux(this.reset.asBool, s_idle, Mux(ifu2WaitWire, s_wait, s_ifu)),
    s_lsu   -> Mux(this.reset.asBool, s_idle, Mux(lsu2WaitWire, s_wait, s_lsu))
  ))

  when(state === s_idle) {
    io.axiLiteSlave.arAddr  := 0.U(32.W)
    io.axiLiteSlave.arValid := 0.U(1.W)
    io.axiLiteSlave.rReady  := 0.U(1.W)
    io.axiLiteSlave.awAddr  := 0.U(32.W)
    io.axiLiteSlave.awValid := 0.U(1.W)
    io.axiLiteSlave.wData   := 0.U(32.W)
    io.axiLiteSlave.wStrb   := 0.U(4.W)
    io.axiLiteSlave.wValid  := 0.U(1.W)
    io.axiLiteSlave.bReady  := 0.U(1.W)
  } .elsewhen(state === s_ifu || state === s_wait) {
    io.axiLiteSlave <> io.axiLiteMaster0
  } .elsewhen(state === s_lsu) {
    io.axiLiteSlave <> io.axiLiteMaster1
  }
}

class AXIBusArbiter extends Module {
  val io = IO(new Bundle {
    val axiSlave0   = Flipped(new AXI)
    val axiSlave1   = Flipped(new AXI)
    val axiMaster   = new AXI
  })
  AXIUtils.initializeAXISlave(io.axiSlave0)
  AXIUtils.initializeAXISlave(io.axiSlave1)
  AXIUtils.initializeAXIMaster(io.axiMaster)

  val awvalidWire0  = io.axiSlave0.awvalid
  val awreadyWire0  = io.axiSlave0.awready
  val wvalidWire0   = io.axiSlave0.wvalid
  val wreadyWire0   = io.axiSlave0.wready
  val bvalidWire0   = io.axiSlave0.bvalid
  val breadyWire0   = io.axiSlave0.bready
  val arvalidWire0  = io.axiSlave0.arvalid
  val arreadyWire0  = io.axiSlave0.arready
  val rvalidWire0   = io.axiSlave0.rvalid
  val rreadyWire0   = io.axiSlave0.rready

  val awvalidWire1  = io.axiSlave1.awvalid
  val awreadyWire1  = io.axiSlave1.awready
  val wvalidWire1   = io.axiSlave1.wvalid
  val wreadyWire1   = io.axiSlave1.wready
  val bvalidWire1   = io.axiSlave1.bvalid
  val breadyWire1   = io.axiSlave1.bready
  val arvalidWire1  = io.axiSlave1.arvalid
  val arreadyWire1  = io.axiSlave1.arready
  val rvalidWire1   = io.axiSlave1.rvalid
  val rreadyWire1   = io.axiSlave1.rready

  val s_idle :: s_wait :: s_ifu :: s_lsu :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val wait2LSUWire  = arvalidWire1.asBool || awvalidWire1.asBool || wvalidWire1.asBool
  val wait2IFUWire  = arvalidWire0.asBool || awvalidWire0.asBool || wvalidWire0.asBool
  val ifu2WaitWire  = (rvalidWire0.asBool && rreadyWire0.asBool) || (wvalidWire0.asBool && wreadyWire0.asBool)
  val lsu2WaitWire  = (rvalidWire1.asBool && rreadyWire1.asBool) || (wvalidWire1.asBool && wreadyWire1.asBool)

  state := MuxLookup(state, s_idle)(List(
    s_idle  -> Mux(this.reset.asBool, s_idle, s_wait),
    s_wait  -> Mux(this.reset.asBool, s_idle, Mux(wait2LSUWire, s_lsu, Mux(wait2IFUWire, s_ifu, s_wait))),
    s_ifu   -> Mux(this.reset.asBool, s_idle, Mux(ifu2WaitWire, s_wait, s_ifu)),
    s_lsu   -> Mux(this.reset.asBool, s_idle, Mux(lsu2WaitWire, s_wait, s_lsu))
  ))

  when(state === s_idle) {
    AXIUtils.initializeAXIMaster(io.axiMaster)
  } .elsewhen(state === s_ifu || state === s_wait) {
    io.axiMaster <> io.axiSlave0
  } .elsewhen(state === s_lsu) {
    io.axiMaster <> io.axiSlave1
  }
}

class AXILiteSram(regMem: Bool) extends Module {
  val io = IO(new Bundle {
    val axiLiteM = Flipped(new AXILite)
  })

  val axiLiteSramV  = Module(new AXILiteSramV)
  val axiLiteReg    = Module(new AXILiteReg(256))

  axiLiteSramV.io.aclk      := false.B
  axiLiteSramV.io.aresetn   := false.B
  axiLiteSramV.io.arAddr    := 0.U
  axiLiteSramV.io.arValid   := false.B
  axiLiteSramV.io.rReady    := false.B
  axiLiteSramV.io.awAddr    := 0.U
  axiLiteSramV.io.awValid   := false.B
  axiLiteSramV.io.wData     := 0.U
  axiLiteSramV.io.wStrb     := 0.U
  axiLiteSramV.io.wValid    := false.B
  axiLiteSramV.io.bReady    := false.B

  axiLiteReg.io := DontCare

  when(regMem) {
    io.axiLiteM <> axiLiteReg.io.axiLite
  } .otherwise {
    axiLiteSramV.io.aclk      := this.clock.asUInt
    axiLiteSramV.io.aresetn   := ~this.reset.asUInt

    /* AR */
    axiLiteSramV.io.arAddr   := io.axiLiteM.arAddr
    axiLiteSramV.io.arValid  := io.axiLiteM.arValid
    io.axiLiteM.arReady      := axiLiteSramV.io.arReady

    /* R */
    io.axiLiteM.rData        := axiLiteSramV.io.rData
    io.axiLiteM.rrEsp        := axiLiteSramV.io.rrEsp
    io.axiLiteM.rValid       := axiLiteSramV.io.rValid
    axiLiteSramV.io.rReady   := io.axiLiteM.rReady

    /* AW */
    axiLiteSramV.io.awAddr   := io.axiLiteM.awAddr
    axiLiteSramV.io.awValid  := io.axiLiteM.awValid
    io.axiLiteM.awReady      := axiLiteSramV.io.awReady

    /* W */
    axiLiteSramV.io.wData    := io.axiLiteM.wData
    axiLiteSramV.io.wStrb    := io.axiLiteM.wStrb
    axiLiteSramV.io.wValid   := io.axiLiteM.wValid
    io.axiLiteM.wReady       := axiLiteSramV.io.wReady

    /* B */
    io.axiLiteM.bResp        := axiLiteSramV.io.bResp
    io.axiLiteM.bValid       := axiLiteSramV.io.bValid
    axiLiteSramV.io.bReady   := io.axiLiteM.bReady
  }
}

class AXILiteSramV extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val aclk    = Input(UInt(1.W))
    val aresetn = Input(UInt(1.W))

    /* AR */
    val arAddr	= Input(UInt(32.W))
    val arValid	= Input(Bool())
    val arReady	= Output(Bool())

    /* R */
    val rData	  = Output(UInt(32.W))
    val rrEsp	  = Output(UInt(2.W))
    val rValid	= Output(Bool())
    val rReady 	= Input(Bool())

    /* AW */
    val awAddr	= Input(UInt(32.W))
    val awValid	= Input(Bool())
    val awReady	= Output(Bool())

    /* W */
    val wData 	= Input(UInt(32.W))
    val wStrb 	= Input(UInt(4.W))
    val wValid 	= Input(Bool())
    val wReady 	= Output(Bool())

    /* B */
    val bResp 	= Output(UInt(2.W))
    val bValid 	= Output(Bool())
    val bReady 	= Input(Bool())
  })

  setInline("AXILiteSramV.sv",
	"""module AXILiteSramV(
	   |  input   aclk,
     |  input   aresetn,
     |  
     |  /* AR */
     |  input [31:0]  arAddr,
     |  input   arValid,
     |  output  arReady,
     |
     |  /* R */
     |  output[31:0]  rData,
     |  output[1:0]   rrEsp,
     |  output        rValid,
     |  input         rReady,
     |
     |  /* AW */
     |  input[31:0]   awAddr,
     |  input         awValid,
     |  output        awReady,
     |
     |  /* W */
     |  input[31:0]   wData,
     |  input[3:0]    wStrb,
     |  input         wValid,
     |  output        wReady,
     |
     |  /* B */
     |  output[1:0]   bResp,
     |  output        bValid,
     |  input         bReady
	   |);
     |
     |/* Headshake Signal Reg */
     |reg       arReadyReg;
     |reg[31:0] rDataReg;
     |reg[1:0]  rrEspReg;
     |reg       rValidReg;
     |reg       awReadyReg;
     |reg       wReadyReg;
     |reg[1:0]  bRespReg;
     |reg       bValidReg;
     |
     |reg       aw_en;
     |reg[31:0] awaddrReg;
     |reg[31:0] araddrReg;
     |
     |assign arReady  = arReadyReg;
     |assign rData    = rDataReg;
     |assign rrEsp    = rrEspReg;
     |assign rValid   = rValidReg;
     |assign awReady  = awReadyReg;
     |assign wReady   = wReadyReg;
     |assign bResp    = bRespReg;
     |assign bValid   = bValidReg;
     |
     |/* AW */
     |always@(posedge aclk) begin
     |  if(!aresetn) begin
     |    awReadyReg  <= 1'b1;
     |    aw_en       <= 1'b1;
     |  end
     |  else if(awValid && wValid && ~awReadyReg && aw_en) begin
     |    awReadyReg  <= 1'b1;
     |    aw_en       <= 1'b0;
     |  end
     |  else if(wValid && wReady) begin
     |    aw_en       <= 1'b1;
     |    awReadyReg  <= 1'b0;
     |  end
     |  else begin
     |    awReadyReg  <= 1'b0;
     |  end
     |end
     |
     |always@(posedge aclk) begin
     |  if(!aresetn) awaddrReg  <= 0;
     |  else if(~awReadyReg && awValid && wValid && aw_en) awaddrReg <= awAddr;
     |  else awaddrReg <= awaddrReg;
     |end
     |
     |/* W */
	   |import "DPI-C" function void pmem_write(
	   |	input int unsigned waddr, input int unsigned wdata, input byte wmask);
     |wire[7:0] wmask;
     |assign wmask = {4'd0, wStrb};
     |always@(posedge aclk) begin
     |  if(!aresetn)                    wReadyReg <= 1'b0;
     |  else if(~wReady && wValid && awValid && aw_en)  wReadyReg <= 1'b1;
     |  else                            wReadyReg <= 1'b0;
     |end
     |always@(posedge aclk) begin
     |  if(wValid && wReady && awValid && awReady)  pmem_write(awAddr, wData, wmask);                   
     |end
     |
     |/* B */
     |always@(posedge aclk) begin
     |  if(!aresetn) begin
     |    bValidReg <= 1'b0;
     |    bRespReg  <= 2'b0;
     |  end
     |  else if(awValid && awReady && wValid && wReady && ~bValidReg) begin
     |    bValidReg <= 1'b1;
     |    bRespReg  <= 2'b0;
     |  end
     |  else if(bValid && bReady) bValidReg <= 1'b0;
     |  else                      bValidReg  <= bValidReg;
     |end
     |
     |/* AR */
     |always@(posedge aclk) begin
     |  if(!aresetn) begin
     |    arReadyReg  <= 1'b1;
     |    araddrReg   <= 32'b0;
     |  end
     |  else if(arValid && ~arReady) begin
     |    arReadyReg  <= 1'b1;
     |    araddrReg   <= arAddr;
     |  end
     |  else begin
     |    arReadyReg  <= 1'b0;
     |    araddrReg   <= araddrReg;
     |  end
     |end
     |
     |/* R */
     |import "DPI-C" function int unsigned pmem_read(input int unsigned raddr, input byte wmask);
     |always@(posedge aclk) begin
     |  if(!aresetn) begin
     |    rValidReg   <= 1'b0;
     |    rrEspReg    <= 2'd0;
     |  end
     |  else if(arValid && arReady && ~rValidReg) begin
     |    rValidReg   <= 1'b1;
     |    rrEspReg    <= 2'd0;
     |  end
     |  else if(rReady) rValidReg   <= 1'b0;
     |  else            rValidReg   <= rValidReg;
     |end
     |always@(posedge aclk) begin
     |  if(!aresetn)begin
     |    rDataReg    <= 32'd0;
     |  end
     |  else if(arValid && arReady && ~rValidReg) begin
     |    rDataReg    <= pmem_read(arAddr, wmask);
     |  end
     |  else begin
     |    rDataReg    <= rDataReg;
     |  end
     |end
     |
	   |endmodule
	""".stripMargin)
}

class AXILiteReg(memorySize: Int) extends Module {
  val io = IO(new Bundle {
    val axiLite = Flipped(new AXILite)
  })
  val mem = RegInit(VecInit(Seq.fill(memorySize)(0.U(32.W))))
  val aresetn = ~this.reset.asUInt

  /* Headshake Signal Reg */
  val arReadyReg  = RegInit(0.B)
  val rDataReg    = RegInit(0.U(32.W))
  val rrEspReg    = RegInit(0.U(2.W))
  val rValidReg   = RegInit(0.B)
  val awReadyReg  = RegInit(1.B)
  val wReadyReg   = RegInit(0.B)
  val bRespReg    = RegInit(0.U(2.W))
  val bValidReg   = RegInit(0.B)

  val awEnReg     = RegInit(1.B)
  val awAddrReg   = RegInit(0.U(32.W))
  val arAddrReg   = RegInit(0.U(32.W))
  
  val wAddrWire   = (awAddrReg - "h80000000".U) >> 2
  val rAddrWire   = (arAddrReg - "h80000000".U) >> 2

  /* Connection */
  /* AR */
  val arAddrWire      = io.axiLite.arAddr
  val arValidWire     = io.axiLite.arValid
  io.axiLite.arReady  := arReadyReg
  /* R */
  io.axiLite.rData    := rDataReg
  io.axiLite.rrEsp    := rrEspReg
  io.axiLite.rValid   := rValidReg
  val rReadyWire      = io.axiLite.rReady
  /* AW */
  val awAddrWire      = io.axiLite.awAddr
  val awValidWire     = io.axiLite.awValid
  io.axiLite.awReady  := awReadyReg
  /* W */
  val wDataWire       = io.axiLite.wData
  val wStrbWire       = io.axiLite.wStrb
  val wValidWire      = io.axiLite.wValid
  io.axiLite.wReady   := wReadyReg
  /* B */
  io.axiLite.bResp    := bRespReg
  io.axiLite.bValid   := bValidReg
  val bReadyWire      = io.axiLite.bReady

  /* AW */
  when (!(aresetn.asBool)) {
    awReadyReg  := 1.B
    awEnReg     := 1.B
  } .elsewhen (awValidWire && wValidWire && (~awReadyReg) && awEnReg) {
    awReadyReg  := 1.B
    awEnReg     := 0.B
  } .elsewhen (wValidWire && wReadyReg) {
    awReadyReg  := 0.B
    awEnReg     := 1.B
  } .otherwise {
    awReadyReg  := 0.B
  }
  when (!(aresetn.asBool)) {
    awAddrReg   := 0.U
  } .elsewhen (awValidWire && wValidWire && (~awReadyReg) && awEnReg) {
    awAddrReg   := awAddrWire
  }
  /* W */
  when(!(aresetn.asBool)) {
    wReadyReg   := 0.B
  } .elsewhen((~wValidWire) && wReadyReg && awValidWire && awReadyReg) {
    wReadyReg   := 1.B
  } .otherwise {
    wReadyReg   := 0.B
  }
  when(wValidWire && wReadyReg && awValidWire && awReadyReg) {
    when (wStrbWire === "b0001".U) {
      mem(rAddrWire(7,0)) := wDataWire & "h000000FF".U
    } .elsewhen (wStrbWire === "b0011".U) {
      mem(rAddrWire(7,0)) := wDataWire & "h0000FFFF".U
    } .elsewhen (wStrbWire === "b1111".U) {
      mem(rAddrWire(7,0)) := wDataWire & "hFFFFFFFF".U
    }
  }
  /* B */
  when(!(aresetn.asBool)) {
    bValidReg   := 1.B
    bRespReg    := 0.U(2.W)
  } .elsewhen (awValidWire && awReadyReg && wValidWire && (~bValidReg)) {
    bValidReg   := 1.B
    bRespReg    := 0.U(2.W)
  } .elsewhen(bValidReg && bReadyWire) {
    bValidReg   := 0.B
  }
  /* AR */
  when(!(aresetn.asBool)) {
    arReadyReg  := 1.B
    arAddrReg   := 0.U(32.W)
  } .elsewhen(arValidWire && (~arReadyReg)) {
    arReadyReg  := 1.B
    arAddrReg   := arAddrWire
  } .otherwise {
    arReadyReg  := 0.B
  }
  /* R */
  when(!(aresetn.asBool)) {
    rValidReg := 0.B
    rrEspReg  := 0.U(2.W)
  } .elsewhen(arValidWire && arReadyReg && (~rValidReg)) {
    rValidReg := 1.B
    rrEspReg  := 0.U(2.W)
  } .elsewhen(rReadyWire) {
    rValidReg := 0.B
  }
  when(!(aresetn.asBool)) {
    rDataReg  := 0.U(32.W)
  } .elsewhen(arValidWire && arReadyReg && (~rValidReg)) {
    when (wStrbWire === "b0001".U) {
      rDataReg := mem(rAddrWire(7,0)) & "h000000FF".U
    } .elsewhen (wStrbWire === "b0011".U) {
      rDataReg := mem(rAddrWire(7,0)) & "h0000FFFF".U
    } .elsewhen (wStrbWire === "b1111".U) {
      rDataReg := mem(rAddrWire(7,0)) & "hFFFFFFFF".U
    }
  }
}

class SkidBuffer(val DATA_WIDTH: Int) extends BlackBox(
    Map("DATA_WIDTH" -> IntParam(DATA_WIDTH))) with HasBlackBoxInline{
    val io = IO(new Bundle {
        val clock       = Input(Clock())
        val reset       = Input(Reset())
        val flush       = Input(Bool())
        
        val input_valid = Input(Bool())
        val input_ready = Output(Bool())
        val input_data  = Input(UInt(DATA_WIDTH.W))

        val output_valid= Output(Bool())
        val output_ready= Input(Bool())
        val output_data = Output(UInt(DATA_WIDTH.W))   
    })

    setInline("SkidBuffer.v",
    """module SkidBuffer
    |#(
    |    parameter DATA_WIDTH = 8
    |)
    |(
    |    input   wire                        clock,
    |    input   wire                        reset,
    |    input   wire                        flush,

    |    input   wire                        input_valid,
    |    output  wire                        input_ready,
    |    input   wire    [DATA_WIDTH-1:0]    input_data,

    |    output  wire                        output_valid,
    |    input   wire                        output_ready,
    |    output  wire    [DATA_WIDTH-1:0]    output_data
    |);

    |/* The A register */
    |reg [DATA_WIDTH-1:0] a_data_q;
    |reg a_full_q;
    |wire a_fill, a_drain;
    |always @(posedge clock) begin
    |    if(reset)       a_data_q <= '0;
    |    else if(a_fill) a_data_q <= input_data;
    |end
    |always @(posedge clock) begin
    |    if(reset)       a_full_q <= '0;
    |    else if(a_fill || a_drain)
    |        a_full_q <= a_fill;
    |end
    |/* Fill the A register when the A or B register is empty. Drain the A register
    |whenever it is full and being filled, or if a flush is requested. */
    |assign a_fill = input_valid && input_ready && (!flush);
    |assign a_drain = (a_full_q && !b_full_q) || flush;

    |/* The B register */
    |reg [DATA_WIDTH-1:0] b_data_q;
    |reg b_full_q;
    |wire b_fill, b_drain;
    |always @(posedge clock) begin
    |    if(reset)       b_data_q <= '0;
    |    else if(b_fill) b_data_q <= a_data_q;
    |end
    |always @(posedge clock) begin
    |    if(reset)       b_full_q <= '0;
    |    else if(b_fill || b_drain)
    |        b_full_q <= b_fill;
    |end
    |/* Fill the B register whenever the A register is drained, but the downstream
    |circuit is not ready. Drain the B register whenever it is full and the
    |downstream circuit is ready, or if a flush is requested. */
    |assign b_fill = a_drain && (!output_ready) && (!flush);
    |assign b_drain = (b_full_q && output_ready) || flush;

    |assign input_ready  = !a_full_q || !b_full_q;
    |assign output_valid = a_full_q | b_full_q;
    |assign output_data  = b_full_q ? b_data_q : a_data_q;
    |endmodule
	  """.stripMargin)
}

class AXISkidBuffer(val AW: Boolean, val W: Boolean, val B: Boolean, val AR: Boolean, val R: Boolean) extends Module {
    val io = IO(new Bundle {
		    val axiMaster   = new AXI
        val axiSlave    = Flipped(new AXI)
    })
    if(AW) {
        val awSkidBuffer    = Module(new SkidBuffer(49))
        awSkidBuffer.io.clock       := clock
        awSkidBuffer.io.reset       := reset
        awSkidBuffer.io.flush       := 0.B

        awSkidBuffer.io.input_valid := io.axiSlave.awvalid
        io.axiSlave.awready         := awSkidBuffer.io.input_ready
        awSkidBuffer.io.input_data  := Cat(io.axiSlave.awaddr, io.axiSlave.awid
        , io.axiSlave.awlen, io.axiSlave.awsize, io.axiSlave.awburst)

        io.axiMaster.awvalid        := awSkidBuffer.io.output_valid
        awSkidBuffer.io.output_ready:= io.axiMaster.awready
        io.axiMaster.awaddr         := awSkidBuffer.io.output_data(48, 17)
        io.axiMaster.awid           := awSkidBuffer.io.output_data(16, 13)
        io.axiMaster.awlen          := awSkidBuffer.io.output_data(12, 5)
        io.axiMaster.awsize         := awSkidBuffer.io.output_data(4, 2)
        io.axiMaster.awburst        := awSkidBuffer.io.output_data(1, 0)
    } else {
        io.axiSlave.awready         := io.axiMaster.awready
        io.axiMaster.awvalid        := io.axiSlave.awvalid
        io.axiMaster.awaddr         := io.axiSlave.awaddr
        io.axiMaster.awid           := io.axiSlave.awid
        io.axiMaster.awlen          := io.axiSlave.awlen
        io.axiMaster.awsize         := io.axiSlave.awsize
        io.axiMaster.awburst        := io.axiSlave.awburst
    }
    if(W) {
        val wSkidBuffer     = Module(new SkidBuffer(37))
        wSkidBuffer.io.clock        := clock
        wSkidBuffer.io.reset       := reset
        wSkidBuffer.io.flush       := 0.B

        wSkidBuffer.io.input_valid  := io.axiSlave.wvalid
        io.axiSlave.wready          := wSkidBuffer.io.input_ready
        wSkidBuffer.io.input_data   := Cat(
        io.axiSlave.wdata, io.axiSlave.wstrb, io.axiSlave.wlast)

        io.axiMaster.wvalid         := wSkidBuffer.io.output_valid
        wSkidBuffer.io.output_ready := io.axiMaster.wready
        io.axiMaster.wdata          := wSkidBuffer.io.output_data(36, 5)
        io.axiMaster.wstrb          := wSkidBuffer.io.output_data(4, 1)
        io.axiMaster.wlast          := wSkidBuffer.io.output_data(0).asBool
    } 
    else {
        io.axiSlave.wready          := io.axiMaster.wready
        io.axiMaster.wvalid         := io.axiSlave.wvalid
        io.axiMaster.wdata          := io.axiSlave.wdata
        io.axiMaster.wstrb          := io.axiSlave.wstrb
        io.axiMaster.wlast          := io.axiSlave.wlast
    }

    if(B) {
        val bSkidBuffer     = Module(new SkidBuffer(6))
        bSkidBuffer.io.clock        := clock
        bSkidBuffer.io.reset        := reset
        bSkidBuffer.io.flush        := 0.B

        bSkidBuffer.io.input_valid  := io.axiMaster.bvalid
        io.axiMaster.bready         := bSkidBuffer.io.input_ready
        bSkidBuffer.io.input_data   := Cat(io.axiMaster.bresp, io.axiMaster.bid)

        io.axiSlave.bvalid          := bSkidBuffer.io.output_valid
        bSkidBuffer.io.output_ready := io.axiSlave.bready
        io.axiSlave.bresp           := bSkidBuffer.io.output_data(5, 4)
        io.axiSlave.bid             := bSkidBuffer.io.output_data(3, 0)
    } else {
        io.axiMaster.bready         := io.axiSlave.bready
        io.axiSlave.bvalid          := io.axiMaster.bvalid
        io.axiSlave.bresp           := io.axiMaster.bresp
        io.axiSlave.bid             := io.axiMaster.bid
    }

    if(AR) {
        val arSkidBuffer    = Module(new SkidBuffer(49))
        arSkidBuffer.io.clock       := clock
        arSkidBuffer.io.reset       := reset
        arSkidBuffer.io.flush       := 0.B

        arSkidBuffer.io.input_valid := io.axiSlave.arvalid
        io.axiSlave.arready         := arSkidBuffer.io.input_ready
        arSkidBuffer.io.input_data  := Cat(io.axiSlave.araddr, io.axiSlave.arid
        , io.axiSlave.arlen, io.axiSlave.arsize, io.axiSlave.arburst)

        io.axiMaster.arvalid        := arSkidBuffer.io.output_valid
        arSkidBuffer.io.output_ready:= io.axiMaster.arready
        io.axiMaster.araddr         := arSkidBuffer.io.output_data(48, 17)
        io.axiMaster.arid           := arSkidBuffer.io.output_data(16, 13)
        io.axiMaster.arlen          := arSkidBuffer.io.output_data(12, 5)
        io.axiMaster.arsize         := arSkidBuffer.io.output_data(4, 2)
        io.axiMaster.arburst        := arSkidBuffer.io.output_data(1, 0)
    } else {
        io.axiSlave.arready         := io.axiMaster.arready
        io.axiMaster.arvalid        := io.axiSlave.arvalid
        io.axiMaster.araddr         := io.axiSlave.araddr
        io.axiMaster.arid           := io.axiSlave.arid
        io.axiMaster.arlen          := io.axiSlave.arlen
        io.axiMaster.arsize         := io.axiSlave.arsize
        io.axiMaster.arburst        := io.axiSlave.arburst
    }

    if(R) {
        val rSkidBuffer     = Module(new SkidBuffer(39))
        rSkidBuffer.io.clock        := clock
        rSkidBuffer.io.reset        := reset
        rSkidBuffer.io.flush        := 0.B

        rSkidBuffer.io.input_valid  := io.axiMaster.rvalid
        io.axiMaster.rready         := rSkidBuffer.io.input_ready
        rSkidBuffer.io.input_data   := Cat(
        io.axiMaster.rresp, io.axiMaster.rdata, io.axiMaster.rlast, io.axiMaster.rid)

        io.axiSlave.rvalid          := rSkidBuffer.io.output_valid
        rSkidBuffer.io.output_ready := io.axiSlave.rready
        io.axiSlave.rresp           := rSkidBuffer.io.output_data(38, 37)
        io.axiSlave.rdata           := rSkidBuffer.io.output_data(36, 5)
        io.axiSlave.rlast           := rSkidBuffer.io.output_data(4).asBool
        io.axiSlave.rid             := rSkidBuffer.io.output_data(3, 0)
    } else {
        io.axiMaster.rready         := io.axiSlave.rready
        io.axiSlave.rvalid          := io.axiMaster.rvalid
        io.axiSlave.rresp           := io.axiMaster.rresp
        io.axiSlave.rdata           := io.axiMaster.rdata
        io.axiSlave.rlast           := io.axiMaster.rlast
        io.axiSlave.rid             := io.axiMaster.rid
    }

}
