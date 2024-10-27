package memory
import chisel3._
import chisel3.util._
import _root_.interface.AXILite

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

class AXILiteInstSram extends Module {
  val io = IO(new Bundle {
    val axiLiteM = Flipped(new AXILite)
  })

  val axiLiteInstSramV  = Module(new AXILiteInstSramV)

  axiLiteInstSramV.io.aclk      := io.axiLiteM.aclk
  axiLiteInstSramV.io.aresetn   := io.axiLiteM.aresetn

  /* AR */
  axiLiteInstSramV.io.arAddr   := io.axiLiteM.arAddr
  axiLiteInstSramV.io.arValid  := io.axiLiteM.arValid
  io.axiLiteM.arReady      := axiLiteInstSramV.io.arReady

  /* R */
  io.axiLiteM.rData        := axiLiteInstSramV.io.rData
  io.axiLiteM.rrEsp        := axiLiteInstSramV.io.rrEsp
  io.axiLiteM.rValid       := axiLiteInstSramV.io.rValid
  axiLiteInstSramV.io.rReady   := io.axiLiteM.rReady

  /* AW */
  axiLiteInstSramV.io.awAddr   := io.axiLiteM.awAddr
  axiLiteInstSramV.io.awValid  := io.axiLiteM.awValid
  io.axiLiteM.awReady      := axiLiteInstSramV.io.awReady

  /* W */
  axiLiteInstSramV.io.wData    := io.axiLiteM.wData
  axiLiteInstSramV.io.wStrb    := io.axiLiteM.wStrb
  axiLiteInstSramV.io.wValid   := io.axiLiteM.wValid
  io.axiLiteM.wReady       := axiLiteInstSramV.io.wReady

  /* B */
  io.axiLiteM.bResp        := axiLiteInstSramV.io.bResp
  io.axiLiteM.bValid       := axiLiteInstSramV.io.bValid
  axiLiteInstSramV.io.bReady   := io.axiLiteM.bReady
}

class AXILiteInstSramV extends BlackBox with HasBlackBoxInline {
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

  setInline("AXILiteInstSramV.sv",
	"""module AXILiteInstSramV(
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
     |  if(!aresetn)                    awReadyReg  <= 1'b1;
     |  else if(awValid && wValid)      awReadyReg  <= 1'b1;
     |  else if(awValid && awReady)     awReadyReg  <= 1'b0;
     |  else                            awReadyReg  <= awReadyReg;
     |end
     |
     |/* W */
     |always@(posedge aclk) begin
     |  if(!aresetn)                    wReadyReg <= 1'b1;
     |  else if(wValid && awValid)      wReadyReg <= 1'b1;
     |  else if(wValid && wReady)       wReadyReg <= 1'b0;
     |  else                            wReadyReg <= wReadyReg;
     |end
     |
     |/* B */
     |always@(posedge aclk) begin
     |  if(!aresetn)                                    bValidReg <= 1'b0;
     |  else if(awValid && awReady && wValid && wReady) bValidReg <= 1'b1;
     |  else if(bValid && bReady)                       bValidReg <= 1'b0;
     |  else                                            bValidReg  <= bValidReg;
     |end
     |always@(posedge aclk) begin
     |  if(!aresetn)                    bRespReg  <= 2'b0;
     |  else if(bValid)                 bRespReg  <= 2'b0;
     |  else                            bRespReg  <= bRespReg;
     |end
     |
     |/* AR */
     |always@(posedge aclk) begin
     |  if(!aresetn)                    arReadyReg  <= 1'b1;
     |  else if(arValid && arReady)     arReadyReg  <= 1'b0;
     |  else if(arValid)                arReadyReg  <= 1'b1; 
     |  else                            arReadyReg  <= arReadyReg;
     |end
     |
     |/* R */
     |import "DPI-C" function int unsigned iaddr_read(int unsigned iaddr);
     |always@(posedge aclk) begin
     |  if(!aresetn)                    rValidReg   <= 1'b0;
     |  else if(arValid && arReady)     rValidReg   <= 1'b1;
     |  else if(rValid && rReady)       rValidReg   <= 1'b0;
     |  else                            rValidReg   <= rValidReg;
     |end
     |always@(posedge aclk) begin
     |  if(!aresetn)begin
     |    rDataReg    <= 32'd0;
     |    rrEspReg    <= 2'd0;
     |  end
     |  else if(arValid && arReady) begin
     |    rDataReg    <= iaddr_read(arAddr);
     |    rrEspReg    <= 2'd0;
     |  end
     |  else begin
     |    rDataReg    <= rDataReg;
     |    rrEspReg    <= rrEspReg;
     |  end
     |end
     |
	   |endmodule
	""".stripMargin)
}

class AXILiteSram extends Module {
  val io = IO(new Bundle {
    val axiLiteM = Flipped(new AXILite)
  })

  val axiLiteSramV  = Module(new AXILiteSramV)

  axiLiteSramV.io.aclk      := io.axiLiteM.aclk
  axiLiteSramV.io.aresetn   := io.axiLiteM.aresetn

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
     |  if(!aresetn)                    awReadyReg  <= 1'b1;
     |  else if(awValid && wValid)      awReadyReg  <= 1'b1;
     |  else if(awValid && awReady)     awReadyReg  <= 1'b0;
     |  else                            awReadyReg  <= awReadyReg;
     |end
     |
     |/* W */
	   |import "DPI-C" function void pmem_write(
	   |	input int unsigned waddr, input int unsigned wdata, input byte wmask);
     |wire[7:0] wmask;
     |assign wmask = {4'd0, wStrb};
     |always@(posedge aclk) begin
     |  if(!aresetn)                    wReadyReg <= 1'b1;
     |  else if(wValid && awValid)      wReadyReg <= 1'b1;
     |  else if(wValid && wReady)       wReadyReg <= 1'b0;
     |  else                            wReadyReg <= wReadyReg;
     |end
     |always@(posedge aclk) begin
     |  if(wValid && wReady && awValid && awReady)  pmem_write(awAddr, wData, wmask);                   
     |end
     |
     |/* B */
     |always@(posedge aclk) begin
     |  if(!aresetn)                                    bValidReg <= 1'b0;
     |  else if(awValid && awReady && wValid && wReady) bValidReg <= 1'b1;
     |  else if(bValid && bReady)                       bValidReg <= 1'b0;
     |  else                                            bValidReg  <= bValidReg;
     |end
     |always@(posedge aclk) begin
     |  if(!aresetn)                    bRespReg  <= 2'b0;
     |  else if(bValid)                 bRespReg  <= 2'b0;
     |  else                            bRespReg  <= bRespReg;
     |end
     |
     |/* AR */
     |always@(posedge aclk) begin
     |  if(!aresetn)                    arReadyReg  <= 1'b1;
     |  else if(arValid)                arReadyReg  <= 1'b1;
     |  else if(arValid && arReady)     arReadyReg  <= 1'b0;
     |  else                            arReadyReg  <= arReadyReg;
     |end
     |
     |/* R */
     |import "DPI-C" function int unsigned pmem_read(input int unsigned raddr);
     |always@(posedge aclk) begin
     |  if(!aresetn)                    rValidReg   <= 1'b0;
     |  if(arValid && arReady)          rValidReg   <= 1'b1;
     |  if(rValid && rReady)            rValidReg   <= 1'b0;
     |  else                            rValidReg   <= rValidReg;
     |end
     |always@(posedge aclk) begin
     |  if(!aresetn)begin
     |    rDataReg    <= 32'd0;
     |    rrEspReg    <= 2'd0;
     |  end
     |  else if(arValid && arReady && rValid && rReady) begin
     |    rDataReg    <= pmem_read(arAddr);
     |    rrEspReg    <= 2'd0;
     |  end
     |  else begin
     |    rDataReg    <= rDataReg;
     |    rrEspReg    <= rrEspReg;
     |  end
     |end
     |
	   |endmodule
	""".stripMargin)
}
