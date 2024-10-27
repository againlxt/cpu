package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
import memory._
//import singlecyclecpu._

/*
class IFU extends Module {
    val io = IO(new Bundle {
		val pc  	 = Input(UInt(32.W))
        //val memData  = Input(UInt(32.W))
        val inst     = Decoupled(new IFU2IDU)
    })
	val instroctionSRAM = Module(new InstroctionSRAM)

	val pcReg 			= RegInit(BigInt("80000000", 16).U(32.W))
	pcReg 			:= io.pc

	instroctionSRAM.io.sramIO.addr 	:= pcReg
	val renReg = RegInit(1.U(1.W))
	instroctionSRAM.io.sramIO.ren 	:= renReg
	when(pcReg =/= io.pc) {
		renReg := 1.U
	} .otherwise {
		renReg := 0.U
	}

	io.inst.valid 	   := instroctionSRAM.io.sramIO.valid
    io.inst.bits.inst  := instroctionSRAM.io.sramIO.data
    io.inst.bits.pc    := pcReg
}
*/

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
	when(clockWire.asBool) {
		when(~resetnWire.asBool) {
			arValidReg := 1.U
		} .elsewhen(arAddrReg =/= io.pc) {
			arValidReg := 1.U
		} .elsewhen(arReadyWire.asBool) {
			arValidReg := 0.U
		}
	}
	when(clockWire.asBool) {
		when(~resetnWire.asBool) {
			rReadyReg := 1.U
		} .elsewhen(rValidWire.asBool && rReadyReg.asBool) {
			rReadyReg := 0.U
		} .elsewhen(rValidWire.asBool) {
			rReadyReg := 1.U
		}
	}

	io.inst.valid		:= rValidWire.asBool && rReadyReg.asBool
	io.inst.bits.inst	:= rDataWire
	io.inst.bits.pc		:= arAddrReg			
}

/*
class InstroctionSRAM extends Module {
	val io = IO(new Bundle {
		val sramIO 		= new IFUSRAM
	})

	val instroctionSRAMV	= Module(new InstroctionSRAMV)
	instroctionSRAMV.io.clk := this.clock.asUInt
	instroctionSRAMV.io.ren	:= io.sramIO.ren
	instroctionSRAMV.io.addr:= io.sramIO.addr
	io.sramIO.valid	:= instroctionSRAMV.io.valid
	io.sramIO.data	:= instroctionSRAMV.io.data
}

class InstroctionSRAMV extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val clk 	= Input(UInt(1.W))
		val ren 	= Input(UInt(1.W))
		val addr 	= Input(UInt(32.W))
		val valid 	= Output(UInt(1.W))
		val data 	= Output(UInt(32.W))
	})

	setInline("InstroctionSRAMV.sv",
	"""module InstroctionSRAMV(
	   |	input	clk,
	   |	input	ren,
	   |	input [31:0] addr,
	   |	output	valid,
	   |	output [31:0] data
	   |);
	   |reg[31:0] rdata;
	   |reg validReg;
	   |import "DPI-C" function int unsigned iaddr_read(int unsigned iaddr);
	   |always@(posedge clk) begin
	   |	if(ren) begin
	   |		rdata 	= iaddr_read(addr);
	   |		validReg= 1'b1;
	   |	end
	   |	else begin
	   |		rdata 	= rdata;
	   |		validReg= 1'b0;
	   |	end
	   |end
	   |
	   |assign valid = validReg;
	   |assign data	 = rdata;
	   |endmodule
	""".stripMargin)
}
*/
