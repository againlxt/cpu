package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
import memory._

class IFU extends Module {
    val io = IO(new Bundle {
		val pc  	 = Input(UInt(32.W))
        //val memData  = Input(UInt(32.W))
        val inst     = Decoupled(new IFU2IDU)
    })
	val instSramAXILite	= Module(new AXILiteInstSram)
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
	instSramAXILite.io.axiLiteM.aclk	:= clockWire
	instSramAXILite.io.axiLiteM.aresetn	:= resetnWire
	/* AR */
	instSramAXILite.io.axiLiteM.arAddr	:= arAddrReg
	instSramAXILite.io.axiLiteM.arValid	:= arValidReg
	val arReadyWire						= instSramAXILite.io.axiLiteM.arReady
	/* R */
	val rDataWire 						= instSramAXILite.io.axiLiteM.rData
	val rrEspWire						= instSramAXILite.io.axiLiteM.rrEsp
	val rValidWire						= instSramAXILite.io.axiLiteM.rValid
	instSramAXILite.io.axiLiteM.rReady	:= rReadyReg
	/* AW */
	instSramAXILite.io.axiLiteM.awAddr	:= awAddrReg
	instSramAXILite.io.axiLiteM.awValid	:= awValidReg
	val awReadyWire						= instSramAXILite.io.axiLiteM.awReady
	/* W */
	instSramAXILite.io.axiLiteM.wData	:= wDataReg
	instSramAXILite.io.axiLiteM.wStrb	:= wStrbReg
	instSramAXILite.io.axiLiteM.wValid	:= wValidReg
	val wReadyWire						= instSramAXILite.io.axiLiteM.wReady
	/* B */
	val bRespWire						= instSramAXILite.io.axiLiteM.bResp
	val bValidWire						= instSramAXILite.io.axiLiteM.bValid
	instSramAXILite.io.axiLiteM.bReady	:= bReadyReg
	
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
