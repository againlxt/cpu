package cpu

import chisel3._
import chisel3.util._
import common._
import cpu.Config
import _root_.interface.WBU2PC

class PC extends Module {
	val io = IO(new Bundle {
		val wbu2PC 		= Flipped(Decoupled(new WBU2PC))
		val pc			= Output(UInt(32.W))
	})

	/* 使用 RegInit 设置 PC 的初始值 */
	val pcReg = RegInit(Mux(Config.SoC.asBool, "h30000000".U(32.W), "h80000000".U(32.W)))
	
	val wbu2PCReadyReg 	= RegInit(1.U(1.W))
	io.wbu2PC.ready 	:= wbu2PCReadyReg
	val pcUpdate		= Module(new PCUpdate)
	val pcUpdateWire 	= io.wbu2PC.ready && io.wbu2PC.valid
	pcUpdate.io.valid	:= pcUpdateWire

	/* 使用 RegNext 更新 PC 值 */
	when (io.wbu2PC.ready && io.wbu2PC.valid) {
		pcReg := io.wbu2PC.bits.nextPC.asUInt
	}

	/* 输出当前 PC 值 */
	io.pc := pcReg
}

class PCUpdate extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle{
		val valid	= Input(Bool())
	})

	setInline("PCUpdate.sv",
	"""module PCUpdate(
	|	input valid
	|);
	|export "DPI-C" function pcUpdate;
	|function bit pcUpdate;
	|	return valid;
	|endfunction
	|endmodule
	""".stripMargin)
}
