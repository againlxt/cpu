package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
import memory._
import javax.management.modelmbean.ModelMBean
import basemode.Delay
import basemode.AXIAccessFault
import cpu.Config
import dpic._
import dpic.PerformanceCounterType.{IFUGETINST => IFUGETINST}
object ReplacePolicy extends ChiselEnum {
  val LRU, FIFO, RANDOM = Value
}
class IFU extends Module {
    val io = IO(new Bundle {
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Icache	= new IFU2Icache
		val wbu2IFU     = Flipped(Decoupled(new WBU2IFU))
    })
	
	val pcReg 		= RegInit(Mux(Config.SoC.asBool, "h30000000".U(32.W), "h80000000".U(32.W)))
	val pcValidReg	= RegNext(io.wbu2IFU.valid & io.wbu2IFU.ready, 1.B)
	val ifuReadyReg	= RegInit(0.B)
	switch(ifuReadyReg) {
		is(0.B) {ifuReadyReg := io.inst.valid & io.inst.ready}
		is(1.B) {ifuReadyReg := !(io.wbu2IFU.valid & io.wbu2IFU.ready)}
	}
	pcReg := Mux(io.wbu2IFU.valid & io.wbu2IFU.ready, io.wbu2IFU.bits.nextPC, pcReg)

	if(!Config.isSTA) {
		val pcUpdate		= Module(new PCUpdate)
		val pcUpdateWire 	= io.wbu2IFU.ready && io.wbu2IFU.valid
		pcUpdate.io.valid	:= pcUpdateWire
	}

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val ifuGetInstCounter = RegInit(0.U(32.W))
		when (io.wbu2IFU.valid & io.wbu2IFU.ready) {
			ifuGetInstCounter := 0.U
		} .otherwise {
			ifuGetInstCounter := ifuGetInstCounter + 1.U
		}
		val IGIC 			= Module(new PerformanceCounter)
		IGIC.io.valid		:= io.ifu2Icache.oEnable
		IGIC.io.counterType	:= PerformanceCounterType.IFUGETINST.asUInt
		IGIC.io.data 		:= ifuGetInstCounter
	}

	io.ifu2Icache.enable:= pcValidReg
	io.ifu2Icache.addr	:= pcReg
	io.inst.valid		:= io.ifu2Icache.oEnable
	io.inst.bits.inst	:= io.ifu2Icache.inst
	io.inst.bits.pc		:= pcReg
	io.wbu2IFU.ready	:= ifuReadyReg
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

class branch extends Module {
	
}
