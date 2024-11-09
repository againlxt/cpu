package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
import memory._
import javax.management.modelmbean.ModelMBean
import basemode.Delay

class IFU extends Module {
    val io = IO(new Bundle {
		val pc  	 	= Input(UInt(32.W))
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Mem		= new AXIMaster 
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
	val breadyReg		= RegInit(0.B)
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
	val awreadyWire		 		= io.ifu2Mem.master_awready
	io.ifu2Mem.master_awvalid	:= awvalidReg
	io.ifu2Mem.master_awaddr	:= awaddrReg
	io.ifu2Mem.master_awid 		:= awidReg
	io.ifu2Mem.master_awlen 	:= awlenReg
	io.ifu2Mem.master_awsize 	:= awsizeReg
	io.ifu2Mem.master_awburst	:= awburstReg
	/* W */
	val wreadyWire 				= io.ifu2Mem.master_wready
	io.ifu2Mem.master_wvalid 	:= wvalidReg
	io.ifu2Mem.master_wdata 	:= wdataReg
	io.ifu2Mem.master_wstrb 	:= wstrbReg
	io.ifu2Mem.master_wlast 	:= wlastReg
	/* B */
	io.ifu2Mem.master_bready	:= breadyReg
	val bvalidWire 				= io.ifu2Mem.master_bvalid
	val brespWire 				= io.ifu2Mem.master_bresp
	val bidWire 				= io.ifu2Mem.master_bid
	/* AR */
	val arreadyWire 			= io.ifu2Mem.master_arready
	io.ifu2Mem.master_arvalid	:= arvalidReg
	io.ifu2Mem.master_araddr	:= araddrReg
	io.ifu2Mem.master_arid 		:= aridReg
	io.ifu2Mem.master_arlen 	:= arlenReg
	io.ifu2Mem.master_arsize 	:= arsizeReg
	io.ifu2Mem.master_arburst	:= arburstReg
	/* R */
	io.ifu2Mem.master_rready 	:= rreadyReg
	val rvalidWire 				= io.ifu2Mem.master_rvalid
	val rrespWire 				= io.ifu2Mem.master_rresp
	val rdataWire 				= io.ifu2Mem.master_rdata
	val rlastWire 				= io.ifu2Mem.master_rlast
	val ridWire 				= io.ifu2Mem.master_rid
	
	/* HeadShake Signals */
	/* AW */
	/* W */
	/* B */
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

	io.inst.valid		:= rvalidWire && rreadyReg
	io.inst.bits.inst	:= rdataWire
	io.inst.bits.pc		:= araddrReg			
}
