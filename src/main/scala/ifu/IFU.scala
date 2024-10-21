package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
//import singlecyclecpu._

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

class InstroctionSRAM extends Module {
	val io = IO(new Bundle {
		val sramIO = new IFUSRAM
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
