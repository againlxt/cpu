package basemode
import chisel3._
import chisel3.util._

class Delay[T <: Data](gen: T, delayCycles: Int) extends Module {
  require(delayCycles >= 0, "延迟周期必须是非负整数")

  val io = IO(new Bundle {
    val in = Input(gen)
    val out = Output(gen)
  })

  // 如果 delayCycles 为 0，则直接输出输入信号
  if (delayCycles == 0) {
    io.out := io.in
  } else {
    // 创建一个寄存器数组来存储延迟信号
    val delayRegs = RegInit(VecInit(Seq.fill(delayCycles)(0.U.asTypeOf(gen))))
    
    // 把输入信号依次传递给寄存器数组
    delayRegs(0) := io.in
    for (i <- 1 until delayCycles) {
      delayRegs(i) := delayRegs(i - 1)
    }
    
    // 输出延迟后的信号
    io.out := delayRegs(delayCycles - 1)
  }
}

class LFSR extends Module {
    val io = IO(new Bundle {
        val clk     = Input(UInt(1.W))
        val rstn    = Input(UInt(1.W))
        val out     = Output(UInt(16.W))
    })
    val lfsrV       = Module(new LFSRV)
    lfsrV.io.clk    := io.clk
    lfsrV.io.rstn   := io.rstn
    io.out          := lfsrV.io.out
}

class LFSRV extends BlackBox with  HasBlackBoxInline {
    val io = IO(new Bundle {
        val clk     = Input(UInt(1.W))
        val rstn    = Input(UInt(1.W))
        val out     = Output(UInt(16.W))
    })

    setInline("LFSRV.sv",
	"""module LFSRV(
	|   input           clk,
    |   input           rstn,
    |   output [15:0]    out
	|);
    |
    |reg[15:0]   outputReg;
    |assign out = outputReg;
    |
    |always@(posedge clk) begin
    |   if(~rstn) outputReg <= 16'hffff;
    |   else      outputReg <= {outputReg[14:0], (outputReg[10]^outputReg[1])};
    |end
	|endmodule
	""".stripMargin)
}
