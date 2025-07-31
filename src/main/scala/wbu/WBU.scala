package wbu

import chisel3._
import chisel3.util._
import memory._
import _root_.interface._
import basemode.AXIAccessFault
import dataclass.data
import basemode.LFSR
import cpu.Config
import dpic._

class WBU extends Module {
	val io = IO(new Bundle {
		val lsu2WBU 	= Flipped(Decoupled(new LSU2WBU))	
        val wbu2CSR     = new WBU2CSR
        val wbu2BaseReg = new WBU2BaseReg
		val wbu2Icache	= Output(Bool())
	})

	val pcReg 			= RegNext(io.lsu2WBU.bits.pc)
    val memDataReg		= RegNext(io.lsu2WBU.bits.memData)
    val aluDataReg		= RegNext(io.lsu2WBU.bits.aluData)
    val csrWDataReg		= RegNext(io.lsu2WBU.bits.csrWData)
    val csrDataReg		= RegNext(io.lsu2WBU.bits.csrData)
    val instReg 		= RegNext(io.lsu2WBU.bits.inst)
    val regWRReg       	= RegNext(io.lsu2WBU.bits.regWR)
    val toRegReg 		= RegNext(io.lsu2WBU.bits.toReg)
    val ecallReg 		= RegNext(io.lsu2WBU.bits.ecall)
    val csrEnReg 		= RegNext(io.lsu2WBU.bits.csrEn)
    val csrWrReg 		= RegNext(io.lsu2WBU.bits.csrWr)
    val fenceiReg		= RegNext(io.lsu2WBU.bits.fencei)
	val handReg 		= RegNext(io.lsu2WBU.valid & io.lsu2WBU.ready)

    /* DPI-C */
	if(!Config.isSTA) {
		val getCurPC	= Module(new GetCurPC)
		val getNextPC 	= Module(new GetNextPC)
        val wbuEnd      = Module(new WBUEnd)
		getCurPC.io.pc 		:= pcReg
		getNextPC.io.nextPC	:= io.lsu2WBU.bits.pc
        wbuEnd.io.handshake := handReg
	}
		
	/* Output */
	io.lsu2WBU.ready	:= 1.B
    io.wbu2CSR.pc       := pcReg
    io.wbu2CSR.csrWData := csrWDataReg
    io.wbu2CSR.csr      := instReg(31,20)
    io.wbu2CSR.ecall    := ecallReg
    io.wbu2CSR.csrEn    := csrEnReg
    io.wbu2CSR.csrWr    := csrWrReg
    io.wbu2BaseReg.data := MuxCase(	0.U(32.W), Seq(	
        (toRegReg === "b00".U).asBool -> aluDataReg,
		(toRegReg === "b01".U).asBool -> memDataReg,
		(toRegReg === "b10".U).asBool -> csrDataReg
    ))
    io.wbu2BaseReg.rdIndex  := instReg(11,7)
    io.wbu2BaseReg.regWR    := regWRReg
	io.wbu2Icache	:= fenceiReg
}

class WBUEnd extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val handshake = Input(Bool())
    })
    setInline("WBUEnd.sv",
	"""module WBUEnd(
	   |  input handshake
	   |);
	   |
	   |export "DPI-C" function wbuEnd;
	   |function bit wbuEnd;
	   |	return handshake;
	   |endfunction
	   |endmodule
	""".stripMargin)
}

class GetCurPC extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val pc = Input(UInt(32.W))
	})
  setInline("GetCurPC.sv",
	"""module GetCurPC(
	   |  input [31:0] pc 
	   |);
	   |
	   |export "DPI-C" function get_cur_pc;
	   |function bit [31:0] get_cur_pc;
	   |	return pc;
	   |endfunction
	   |endmodule
	""".stripMargin)
}

class GetNextPC extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val nextPC = Input(UInt(32.W))
	})
  setInline("GetNextPC.sv",
	"""module GetNextPC(
	   |  input	[31:0] nextPC
	   |);
	   |
	   |export "DPI-C" function get_next_pc;
	   |function bit [31:0] get_next_pc;
	   |	return nextPC;
	   |endfunction
	   |endmodule
	""".stripMargin)
}
