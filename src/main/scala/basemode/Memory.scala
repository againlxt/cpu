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

class AXILiteSram extends Module {
  val io = IO(new Bundle {
    val axiLiteM = new AXILite
  })

  val axiLiteSramV  = Module(new AXILiteSramV)

  axiLiteSramV.io.clk      := this.clock.asUInt
  axiLiteSramV.io.reset    := this.reset.asUInt

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
     |reg[31:0] rDataReg;
     |reg[1:0]  rrEspReg;
     |reg       rValidReg;
     |reg       awReadyReg;
     |reg       wReadyReg;
     |reg[1:0]  bRespReg;
     |reg       bValidReg;
     |
     |
	   |endmodule
	""".stripMargin)
}
