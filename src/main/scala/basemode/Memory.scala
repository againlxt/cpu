package memory
import chisel3._
import chisel3.util._

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

class RegistMem(dataWidth: Int, addrWidth: Int, memorySize: Int) extends Module {
  val io = IO(new Bundle {} {
    val enable  = Input(Bool())
    val write   = Input(Bool())
    val addr    = Input(UInt(addrWidth.W))
    val baseAddr= Input(UInt(32.W))
    val len     = Input(UInt(3.W))            // len 可以是 1, 2, 或 4
    val dataIn  = Input(UInt(dataWidth.W))
    val dataOut = Output(UInt(dataWidth.W))
  })

  val mem = RegInit(VecInit(Seq.fill(256)(0.U(32.W))))

  io.dataOut := 0.U

  val addrWire  = (io.addr - io.baseAddr) >> 2
  val dataWire  = io.dataIn 

  when(io.enable) {
    when(io.write) {
      when(io.len === 1.U) {
        mem(addrWire) := dataWire(7, 0)
      } .elsewhen (io.len === 2.U) {
        mem(addrWire) := dataWire(15, 0)
      } .elsewhen (io.len === 4.U) {
        mem(addrWire) := dataWire(31, 0)
      }
    } .otherwise {
      val memDataWire  = mem(addrWire)
      when(io.len === 1.U) {
        io.dataOut  := memDataWire(7, 0)
      } .elsewhen (io.len === 2.U) {
        io.dataOut  := memDataWire(15, 0)
      } .elsewhen (io.len === 4.U) {
        io.dataOut  := memDataWire(31, 0)
      }
    }
  }
}
