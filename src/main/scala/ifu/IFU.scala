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
class IFU extends Module {
    val io = IO(new Bundle {
		val pc  	 	= Input(UInt(32.W))
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Mem		= new AXI 
    })
	/* Clock and Reset */
	val clockWire		= this.clock.asUInt
	val resetnWire		= ~this.reset.asUInt

	/* AW */
	val awvalidReg		= RegInit(0.B)
	val awaddrReg		= RegInit(0.U(32.W))
	val awidReg 		= RegInit(0.U(4.W))
	val awlenReg 		= RegInit(0.U(8.W))
	val awsizeReg 		= RegInit(2.U(3.W))
	val awburstReg 		= RegInit(1.U(2.W))
	/* W */
	val wvalidReg		= RegInit(0.B)
	val wdataReg		= RegInit(0.U(32.W))
	val wstrbReg		= RegInit(15.U(4.W))
	val wlastReg 		= RegInit(0.B)
	/* B */
	val breadyReg		= RegInit(1.B)
	/* AR */
	val arvalidReg		= RegInit(1.U(1.W))
	val araddrReg		= RegInit(BigInt("80000000", 16).U(32.W))
	val aridReg 		= RegInit(0.U(4.W))
	val arlenReg 		= RegInit(0.U(8.W))
	val arsizeReg 		= RegInit(2.U(3.W))
	val arburstReg 		= RegInit(1.U(2.W))
	araddrReg			:= io.pc
	/* R */
	val rreadyReg		= RegInit(0.B)

	/* Signal Connection */
	/* AW */
	val awreadyWire		 = io.ifu2Mem.awready
	io.ifu2Mem.awvalid	:= awvalidReg
	io.ifu2Mem.awaddr	:= awaddrReg
	io.ifu2Mem.awid 	:= awidReg
	io.ifu2Mem.awlen 	:= awlenReg
	io.ifu2Mem.awsize 	:= awsizeReg
	io.ifu2Mem.awburst	:= awburstReg
	/* W */
	val wreadyWire 		= io.ifu2Mem.wready
	io.ifu2Mem.wvalid 	:= wvalidReg
	io.ifu2Mem.wdata 	:= wdataReg
	io.ifu2Mem.wstrb 	:= wstrbReg
	io.ifu2Mem.wlast 	:= wlastReg
	/* B */
	io.ifu2Mem.bready	:= breadyReg
	val bvalidWire 		= io.ifu2Mem.bvalid
	val brespWire 		= io.ifu2Mem.bresp
	val bidWire 		= io.ifu2Mem.bid
	/* AR */
	val arreadyWire 	= io.ifu2Mem.arready
	io.ifu2Mem.arvalid	:= arvalidReg
	io.ifu2Mem.araddr	:= araddrReg
	io.ifu2Mem.arid 	:= aridReg
	io.ifu2Mem.arlen 	:= arlenReg
	io.ifu2Mem.arsize 	:= arsizeReg
	io.ifu2Mem.arburst	:= arburstReg
	/* R */
	io.ifu2Mem.rready 	:= rreadyReg
	val rvalidWire 		= io.ifu2Mem.rvalid
	val rrespWire 		= io.ifu2Mem.rresp
	val rdataWire 		= io.ifu2Mem.rdata
	val rlastWire 		= io.ifu2Mem.rlast
	val ridWire 		= io.ifu2Mem.rid
	
	/* HeadShake Signals */
	/* AW */
	/* W */
	/* B */
	if(Config.hasDPIC & (!Config.isSTA)) {
		val axiAccessFault = Module(new AXIAccessFault())
		axiAccessFault.io.ready := breadyReg
		axiAccessFault.io.valid := bvalidWire
		axiAccessFault.io.resp	:= brespWire
	}
	when(~resetnWire.asBool) {
		breadyReg	:= 1.U
	} .elsewhen(bvalidWire && io.ifu2Mem.bready) {
		breadyReg	:= 0.U
	} .elsewhen(bvalidWire) {
		breadyReg	:= 1.U
	}
	/* AR */
	when(~resetnWire.asBool) {
		arvalidReg := 1.U
	} .elsewhen(araddrReg =/= io.pc) {
		arvalidReg := 1.U
	} .elsewhen(arreadyWire.asBool) {
		arvalidReg := 0.U
	}
	/* R */
	when(~resetnWire.asBool) {
		rreadyReg := 1.U
	} .elsewhen(rvalidWire.asBool && rreadyReg.asBool) {
		rreadyReg := 0.U
	} .elsewhen(rvalidWire.asBool) {
		rreadyReg := 1.U
	}

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val ifuGetInstCounter = RegInit(0.U(32.W))
		when (arvalidReg.asBool && arreadyWire.asBool) {
			ifuGetInstCounter := 0.U
		} .otherwise {
			ifuGetInstCounter := ifuGetInstCounter + 1.U
		}
		val IGIC 			= Module(new PerformanceCounter)
		IGIC.io.valid		:= rvalidWire.asBool && rreadyReg.asBool
		IGIC.io.counterType	:= PerformanceCounterType.IFUGETINST.asUInt
		IGIC.io.data 		:= ifuGetInstCounter
	}

	io.inst.valid		:= rvalidWire && rreadyReg
	io.inst.bits.inst	:= rdataWire
	io.inst.bits.pc		:= araddrReg			
}
