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
        //val memData  = Input(UInt(32.W))
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Mem		= new AXILite 
    })
	/* Clock and Reset */
	val clockWire		= this.clock.asUInt
	val resetnWire		= ~this.reset.asUInt

	/* AW */
	val awAddrReg		= RegInit(0.U(32.W))
	val awValidReg		= RegInit(0.U(1.W))
	/* W */
	val wDataReg		= RegInit(0.U(32.W))
	val wStrbReg		= RegInit(15.U(4.W))
	val wValidReg		= RegInit(0.U(1.W))
	/* B */
	val bReadyReg		= RegInit(0.U(1.W))
	/* AR */
	val arAddrReg		= RegInit(BigInt("80000000", 16).U(32.W))
	arAddrReg			:= io.pc
	val arValidReg		= RegInit(1.U(1.W))
	/* R */
	val rReadyReg		= RegInit(1.U(1.W))	

	/* Signal Connection */
	/* Clock And Reset */
	io.ifu2Mem.aclk	:= clockWire
	io.ifu2Mem.aresetn	:= resetnWire
	/* AR */
	io.ifu2Mem.arAddr	:= arAddrReg
	io.ifu2Mem.arValid	:= arValidReg
	val arReadyWire						= io.ifu2Mem.arReady
	/* R */
	val rDataWire 						= io.ifu2Mem.rData
	val rrEspWire						= io.ifu2Mem.rrEsp
	val rValidWire						= io.ifu2Mem.rValid
	io.ifu2Mem.rReady	:= rReadyReg
	/* AW */
	io.ifu2Mem.awAddr	:= awAddrReg
	io.ifu2Mem.awValid	:= awValidReg
	val awReadyWire						= io.ifu2Mem.awReady
	/* W */
	io.ifu2Mem.wData	:= wDataReg
	io.ifu2Mem.wStrb	:= wStrbReg
	io.ifu2Mem.wValid	:= wValidReg
	val wReadyWire						= io.ifu2Mem.wReady
	/* B */
	val bRespWire						= io.ifu2Mem.bResp
	val bValidWire						= io.ifu2Mem.bValid
	io.ifu2Mem.bReady	:= bReadyReg
	
	/* HeadShake Signals */
	when(~resetnWire.asBool) {
		arValidReg := 1.U
	} .elsewhen(arAddrReg =/= io.pc) {
		arValidReg := 1.U
	} .elsewhen(arReadyWire.asBool) {
		arValidReg := 0.U
	}
	when(~resetnWire.asBool) {
		rReadyReg := 1.U
	} .elsewhen(rValidWire.asBool && rReadyReg.asBool) {
		rReadyReg := 0.U
	} .elsewhen(rValidWire.asBool) {
		rReadyReg := 1.U
	}

	io.inst.valid		:= rValidWire.asBool && rReadyReg.asBool
	io.inst.bits.inst	:= rDataWire
	io.inst.bits.pc		:= arAddrReg			
}
