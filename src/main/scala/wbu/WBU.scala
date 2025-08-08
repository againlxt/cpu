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
		val rd 			= Output(UInt(4.W))
		val flush 		= Output(Bool())
		val correctPC 	= Output(UInt(32.W))
	})
    val handWire        = io.lsu2WBU.valid & io.lsu2WBU.ready
	val pcWire 			= io.lsu2WBU.bits.pc
    val memDataWire		= io.lsu2WBU.bits.memData
    val aluDataWire		= io.lsu2WBU.bits.aluData
    val csrWDataWire	= io.lsu2WBU.bits.csrWData
    val csrDataWire		= io.lsu2WBU.bits.csrData
    val instWire 		= io.lsu2WBU.bits.inst
    val regWRWire       = io.lsu2WBU.bits.regWR
    val toRegWire 		= io.lsu2WBU.bits.toReg
    val ecallWire 		= io.lsu2WBU.bits.ecall
    val csrEnWire 		= io.lsu2WBU.bits.csrEn
    val csrWrWire 		= io.lsu2WBU.bits.csrWr
    val fenceiWire		= io.lsu2WBU.bits.fencei
	val handReg 		= RegNext(handWire)
	val handRReg 		= RegNext(handReg)

	val s_flow :: s_flush :: Nil = Enum(2)
	val nextState	= WireInit(s_flow)
	val state 		= RegNext(nextState, s_flow)
	nextState	:= MuxLookup(state, s_flow)(List(
		s_flow 	-> Mux(ecallWire.asBool, s_flush, s_flow),
		s_flush	-> Mux(handWire, s_flow, s_flush)
	))

    /* DPI-C */
	if(!Config.isSTA) {
		val getCurPC	= Module(new GetCurPC)
		val getNextPC 	= Module(new GetNextPC)
		val getCmd 		= Module(new GetCmd)
        val wbuEnd      = Module(new WBUEnd)
		getCurPC.io.pc 		:= pcWire
		getNextPC.io.nextPC	:= io.lsu2WBU.bits.pc
		getCmd.io.cmd 		:= instWire
        wbuEnd.io.handshake := handReg
		val skipDiff = Module(new SkipDiff())
        skipDiff.io.en := handWire & io.lsu2WBU.bits.skip
	}
		
	/* Output */
	io.lsu2WBU.ready	:= 1.B
    io.wbu2CSR.pc       := pcWire
    io.wbu2CSR.csrWData := csrWDataWire
    io.wbu2CSR.csr      := instWire(31,20)
    io.wbu2CSR.ecall    := ecallWire
    io.wbu2CSR.csrEn    := csrEnWire
    io.wbu2CSR.csrWr    := csrWrWire
    io.wbu2BaseReg.data := MuxCase(	0.U(32.W), Seq(	
        (toRegWire === "b00".U).asBool -> aluDataWire,
		(toRegWire === "b01".U).asBool -> memDataWire,
		(toRegWire === "b10".U).asBool -> csrDataWire
    ))
    io.wbu2BaseReg.rdIndex  := instWire(11,7)
    io.wbu2BaseReg.regWR    := regWRWire
	io.wbu2BaseReg.pc		:= pcWire
	io.wbu2Icache	:= fenceiWire
	io.rd		:= Mux(regWRWire.asBool, Mux(handReg, instWire(11,7), 0.U), 0.U)

	io.flush	:= (state === s_flow) & (nextState === s_flush)
	io.correctPC:= io.wbu2CSR.mtvec
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

class GetCmd extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val cmd = Input(UInt(32.W))
	})
  setInline("GetCmd.sv",
	"""module GetCmd(
	   |  input [31:0] cmd
	   |);
	   |import "DPI-C" function void sim_exit();
		|always @(cmd) begin
		|    if(cmd==32'h00100073)   sim_exit();
		|end
	   |export "DPI-C" function getCommond;
	   |function bit [31:0] getCommond;
	   |	return cmd;
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

class SkipDiff extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val en = Input(Bool())
    })

	setInline("SkipDiff.sv",
	"""module SkipDiff(
	|	input en
	|);
	|
	|import "DPI-C" function void difftest_skip_ref();
	|always@(en) begin
	|	if(en) difftest_skip_ref();
	|end
	|
	|endmodule
	""".stripMargin)
}
