package lsu

import chisel3._
import chisel3.util._
import _root_.interface._
import cpu.Config
import dpic._
import basemode.AXIAccessFault

class LSU extends Module {
    val io = IO(new Bundle {
        val exu2LSU     = Flipped(Decoupled(new EXU2LSU))
        val lsu2Mem     = new AXI
        val lsu2WBU     = Decoupled(new LSU2WBU)
		val lsu2BaseReg = new LSU2BaseReg
    })
	val pcWire 			= io.exu2LSU.bits.pc
	val memDataWire		= io.exu2LSU.bits.memData
	val aluDataWire		= io.exu2LSU.bits.aluData
	val csrWDataWire		= io.exu2LSU.bits.csrWData
	val csrDataWire		= io.exu2LSU.bits.csrData
	val immDataWire 	    = io.exu2LSU.bits.immData
	val rs1DataWire 	    = io.exu2LSU.bits.rs1Data
	val instWire 		= io.exu2LSU.bits.inst
	val regWRWire        = io.exu2LSU.bits.regWR
	val memWRWire 		= io.exu2LSU.bits.memWR
	val memValidWire	    = io.exu2LSU.bits.memValid
	val memOPWire 		= io.exu2LSU.bits.memOP
	val toRegWire 		= io.exu2LSU.bits.toReg
	val ecallWire 		= io.exu2LSU.bits.ecall
	val csrEnWire 		= io.exu2LSU.bits.csrEn
	val csrWrWire 		= io.exu2LSU.bits.csrWr

    val wMaskWire 	= MuxCase (1.U(4.W), Seq(
		(memOPWire(1,0) === 0.U).asBool -> "b0001".U,
		(memOPWire(1,0) === 1.U).asBool -> "b0011".U,
		(memOPWire(1,0) === 2.U).asBool -> "b1111".U
	))
	val sOrUWire 	= MuxCase (0.U(1.W), Seq(
		(memOPWire(2) === 0.U).asBool -> 1.U(1.W),
		(memOPWire(2) === 1.U).asBool -> 0.U(1.W)
	))

    /* AXI Transport */
	/* AW */
	val awreadyWire 	= io.lsu2Mem.awready
	val awvalidReg 		= RegInit(0.B)
	io.lsu2Mem.awvalid 	:= awvalidReg
	io.lsu2Mem.awaddr	:= aluDataWire
	val awidReg 		= RegInit(0.U(4.W))
	io.lsu2Mem.awid 	:= awidReg
	val awlenReg 		= RegInit(0.U(8.W))
	io.lsu2Mem.awlen 	:= awlenReg
	io.lsu2Mem.awsize 	:= memOPWire(1,0)
	val awburstReg 		= RegInit(1.U(2.W))
	io.lsu2Mem.awburst 	:= awburstReg
	/* W */
	val wreadyWire 		= io.lsu2Mem.wready
	val wvalidReg 		= RegInit(0.B)
	io.lsu2Mem.wvalid 	:= wvalidReg
	if (Config.SoC) {
		io.lsu2Mem.wdata 	:= MuxCase(memDataWire, Seq(
			(aluDataWire(1,0) === 0.U && memOPWire(1,0) === 2.U).asBool	-> memDataWire,
			(aluDataWire(1,0) === 0.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), memDataWire(15,0)),
			(aluDataWire(1,0) === 0.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), memDataWire(7,0)),
			(aluDataWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), Cat(memDataWire(7,0), 0.U(8.W))),
			(aluDataWire(1,0) === 2.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(8.W), Cat(memDataWire(7,0), 0.U(16.W))),
			(aluDataWire(1,0) === 2.U && memOPWire(1,0) === 1.U).asBool	-> Cat(memDataWire(15,0), 0.U(16.W)),
			(aluDataWire(1,0) === 3.U).asBool	-> Cat(memDataWire(7,0), 0.U(24.W))
		))
		io.lsu2Mem.wstrb 	:= MuxCase(wMaskWire, Seq(
			(aluDataWire(1,0) === 0.U & memWRWire.asBool).asBool	-> wMaskWire,
			(aluDataWire(1,0) === 1.U & memWRWire.asBool).asBool	-> Cat(wMaskWire(2,0), 0.U(1.W)),
			(aluDataWire(1,0) === 2.U & memWRWire.asBool).asBool	-> Cat(wMaskWire(1,0), 0.U(2.W)),
			(aluDataWire(1,0) === 3.U & memWRWire.asBool).asBool	-> Cat(wMaskWire(0), 0.U(3.W))
		))
	} else {
		io.lsu2Mem.wdata	:= memDataWire
		io.lsu2Mem.wstrb	:= wMaskWire
	}
	val wlastReg 		= RegInit(0.B)
	io.lsu2Mem.wlast 	:= wlastReg
	/* B */
	val breadyReg 		= RegInit(1.B)
	io.lsu2Mem.bready 	:= breadyReg
	val bvalidWire 		= io.lsu2Mem.bvalid
	val brespWire 		= io.lsu2Mem.bresp
	val bidWire 		= io.lsu2Mem.bid
    if(Config.hasDPIC & (!Config.isSTA)) {
		val axiAccessFault = Module(new AXIAccessFault())
		axiAccessFault.io.ready := breadyReg
		axiAccessFault.io.valid := bvalidWire
		axiAccessFault.io.resp	:= brespWire
	}
	/* AR */
	val arreadyWire 	= io.lsu2Mem.arready
	val arvalidReg 		= RegInit(0.B)
	io.lsu2Mem.arvalid 	:= arvalidReg
	io.lsu2Mem.araddr	:= aluDataWire
	val aridReg 		= RegInit(0.U(4.W))
	io.lsu2Mem.arid 	:= aridReg
	val arlenReg 		= RegInit(0.U(8.W))
	io.lsu2Mem.arlen 	:= arlenReg
	io.lsu2Mem.arsize 	:= memOPWire(1,0) 
	val arburstReg 		= RegInit(1.U(2.W))
	io.lsu2Mem.arburst 	:= arburstReg
	/* R */
	val rreadyReg 		= RegInit(1.B)
	io.lsu2Mem.rready 	:= rreadyReg
	val rvalidWire 		= io.lsu2Mem.rvalid
	val rrespWire 		= io.lsu2Mem.rresp
	val rdataWire 		= io.lsu2Mem.rdata
	val rdataReg 		= RegInit(0.U(32.W))
	val rdataShiftWire  = Wire(UInt(32.W))
	val signDataWire 	= Wire(SInt(32.W))
	if(Config.SoC) {
		rdataShiftWire 	:= MuxCase(rdataReg, Seq(
			/* MROM Read */
			((aluDataWire <= "h20000fff".U) && (aluDataWire >= "h20000000".U) && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataWire <= "h20000fff".U) && (aluDataWire >= "h20000000".U) && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataWire <= "h20000fff".U) && (aluDataWire >= "h20000000".U) && memOPWire(1,0) === 2.U).asBool	-> rdataReg,

			/* FLASH Read */
			((aluDataWire <= "h3fffffff".U) && (aluDataWire >= "h30000000".U) && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataWire <= "h3fffffff".U) && (aluDataWire >= "h30000000".U) && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataWire <= "h3fffffff".U) && (aluDataWire >= "h30000000".U) && memOPWire(1,0) === 2.U).asBool	-> rdataReg,

			/* PSRAM Read */
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 2.U).asBool -> rdataReg,
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 1.U).asBool	-> Cat(0.U(24.W), rdataReg(15,8)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(23,16)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(31,16)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 3.U).asBool	-> Cat(0.U(24.W), rdataReg(31,24)),
			
			/* SRAM Read */
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 2.U).asBool -> rdataReg,
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 1.U).asBool	-> Cat(0.U(24.W), rdataReg(15,8)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(23,16)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(31,16)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 3.U).asBool	-> Cat(0.U(24.W), rdataReg(31,24)),
		))
		signDataWire					:= MuxCase(rdataShiftWire.asSInt, Seq(
			(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, rdataShiftWire(7)), rdataShiftWire(7, 0)).asSInt,
			(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, rdataShiftWire(15)), rdataShiftWire(15, 0)).asSInt,
			(wMaskWire === "b1111".U).asBool 	-> rdataShiftWire.asSInt
		))
	} else {
		rdataShiftWire 	:= MuxCase(rdataReg, Seq(
			(wMaskWire === "b0001".U).asBool 	-> Cat(0.U(24.W), rdataReg(7, 0)),
			(wMaskWire === "b0011".U).asBool 	-> Cat(0.U(16.W), rdataReg(15, 0)),
			(wMaskWire === "b1111".U).asBool 	-> rdataReg
		))
		signDataWire					:= MuxCase(rdataReg.asSInt, Seq(
			(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, rdataReg(7)), rdataReg(7, 0)).asSInt,
			(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, rdataReg(15)), rdataReg(15, 0)).asSInt,
			(wMaskWire === "b1111".U).asBool 	-> rdataReg.asSInt
		))
	}
	val memRdDataWire 	= Mux(sOrUWire.asBool, signDataWire.asUInt, rdataShiftWire)

    val s_wait_valid :: s_write :: s_read :: s_wait_ready :: Nil = Enum(4)
    val state   = RegInit(s_wait_valid)
    val memEnd  = (io.lsu2Mem.bvalid & io.lsu2Mem.bready) || 
    (io.lsu2Mem.rvalid & io.lsu2Mem.rready & io.lsu2Mem.rlast)
    state       := MuxLookup(state, s_wait_valid)(List(
        s_wait_valid  	-> Mux(io.exu2LSU.ready & io.exu2LSU.valid, 
        Mux(io.exu2LSU.bits.memValid.asBool, 
        Mux(io.exu2LSU.bits.memWR.asBool, s_write, s_read), s_wait_valid), s_wait_valid),
        s_write 		-> Mux(memEnd, s_wait_ready, s_write),
        s_read  		-> Mux(memEnd, s_wait_ready, s_read),
		s_wait_ready	-> Mux(io.lsu2WBU.ready & io.lsu2WBU.valid, s_wait_valid, s_wait_ready)
    ))
    val wOpWire      = io.exu2LSU.ready & io.exu2LSU.valid & 
    io.exu2LSU.bits.memValid.asBool & io.exu2LSU.bits.memWR.asBool
    val rOpWire      = io.exu2LSU.ready & io.exu2LSU.valid & 
    io.exu2LSU.bits.memValid.asBool & (!io.exu2LSU.bits.memWR.asBool)
    switch(state) {
        is(s_wait_valid) {
            awvalidReg  := wOpWire
            wvalidReg   := wOpWire
            wlastReg    := wOpWire
            arvalidReg  := rOpWire
            rreadyReg   := rOpWire
        }
        is(s_write) {
			switch(awvalidReg) {
				is(1.B) {awvalidReg := (!(io.lsu2Mem.awvalid & io.lsu2Mem.awready)) & (!memEnd)}
				is(0.B)	{awvalidReg := 0.B}
			}
            wvalidReg   := !memEnd
            wlastReg    := !memEnd
        }
        is(s_read) {
			switch(arvalidReg) {
				is(1.B) {arvalidReg := !(io.lsu2Mem.arvalid & io.lsu2Mem.arready)}
				is(0.B) {arvalidReg := 0.B}
			}
            rreadyReg   := !memEnd
            rdataReg    := Mux(memEnd, rdataWire, rdataReg)
        }
    }

    /* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val lsuGetDataCnt = RegInit(0.U(32.W))
		when (state === s_read) {
			lsuGetDataCnt := lsuGetDataCnt + 1.U
		} .otherwise {
			lsuGetDataCnt := 0.U
		}
		val LGDC 			= Module(new PerformanceCounter)
		LGDC.io.valid		:= memEnd
		LGDC.io.counterType	:= PerformanceCounterType.LSUGETDATA.asUInt
		LGDC.io.data 		:= lsuGetDataCnt+1.U
	}
    /* DPIC */
	if(!Config.isSTA) {
		val getCmd 			= Module(new GetCommond)
		getCmd.io.cmd 		:= instWire
	}
	if(Config.hasDPIC & (!Config.isSTA)) {
		val mTrace 			= Module(new MTrace)
		mTrace.io.data 		:= Mux(memWRWire.asBool, io.lsu2Mem.wdata, Mux(sOrUWire.asBool, signDataWire.asUInt, rdataShiftWire))
		mTrace.io.addr 		:= aluDataWire
		mTrace.io.memop 	:= memOPWire(1,0)
		mTrace.io.wOrR 		:= memWRWire.asBool
		mTrace.io.enable	:= memEnd
	}

    /* IO */
    io.exu2LSU.ready            := (state === s_wait_valid)
    io.lsu2WBU.valid            := (state === s_wait_ready) ||
	(io.exu2LSU.ready & io.exu2LSU.valid & (!io.exu2LSU.bits.memValid.asBool))
    io.lsu2WBU.bits.pc          := pcWire
    io.lsu2WBU.bits.memData     := memRdDataWire
    io.lsu2WBU.bits.aluData     := aluDataWire
    io.lsu2WBU.bits.csrWData    := csrWDataWire
    io.lsu2WBU.bits.csrData     := csrDataWire
    io.lsu2WBU.bits.rs1Data     := rs1DataWire
    io.lsu2WBU.bits.inst        := instWire
    io.lsu2WBU.bits.regWR       := regWRWire
    io.lsu2WBU.bits.toReg       := toRegWire
    io.lsu2WBU.bits.ecall       := ecallWire
    io.lsu2WBU.bits.csrEn       := csrEnWire
    io.lsu2WBU.bits.csrWr       := csrWrWire
    io.lsu2WBU.bits.fencei      := (memOPWire === 7.U)
	io.lsu2BaseReg.rdIndex 		:= instWire(11,7)
	io.lsu2BaseReg.regWR 		:= regWRWire
	io.lsu2BaseReg.pc			:= pcWire
	io.lsu2BaseReg.handShake	:= io.lsu2WBU.valid & io.lsu2WBU.ready
}

class GetCommond extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle{
		val cmd	= Input(UInt(32.W))
	})

	setInline("GetCommond.sv",
	"""module GetCommond(
	|	input [31:0] cmd
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

class MTrace extends BlackBox with  HasBlackBoxInline {
	val io = IO(new Bundle{
		val data	= Input(UInt(32.W))
		val addr	= Input(UInt(32.W))
		val memop	= Input(UInt(2.W))
		val wOrR 	= Input(Bool())
		val enable  = Input(Bool())
	})

	setInline("MTrace.sv",
	"""module MTrace(
	|	input [31:0] data,
	|	input [31:0] addr,
	|	input [1:0] memop,
	|	input wOrR,
	|	input enable
	|);
	|wire [7:0] memop_w, wOrR_w;
	|assign memop_w = {6'd0, memop};
	|assign wOrR_w 	= {7'd0, wOrR};
	|import "DPI-C" function void MTrace(input int data, input int addr, input byte memop, input byte wOrR);
	|always @(posedge enable)   MTrace(data, addr, memop_w, wOrR_w);
	|endmodule
	""".stripMargin)
}

class BranchCond extends Module {
	val io = IO(new Bundle {
		// Input
		val branch 	= Input(UInt(4.W))
		val less 	= Input(Bool())
		val zero 	= Input(Bool())

		// Output
		val pcASrc 	= Output(UInt(2.W))
		val pcBSrc 	= Output(UInt(2.W))
	})

	val branchWire 	= io.branch
	val lessWire 	= io.less
	val zeroWire 	= io.zero

	io.pcASrc := MuxCase(0.U, Seq(
		(branchWire === "b0000".U) -> 0.U,
		(branchWire === "b0001".U) -> 1.U,
		(branchWire === "b0010".U) -> 1.U,
		(branchWire === "b0100".U) -> zeroWire,
		(branchWire === "b0101".U) -> (!zeroWire).asUInt,
		(branchWire === "b0110".U) -> lessWire,
		(branchWire === "b0111".U) -> (!lessWire).asUInt,
		(branchWire === "b1000".U) -> 2.U
	))
	

	io.pcBSrc := MuxCase(0.U, Seq(
		(branchWire === "b1000".U) -> 2.U,  // 最高优先级
		(branchWire === "b0010".U) -> 1.U,  // 只有b0010返回1
		(branchWire =/= "b1000".U) -> 0.U  // 其他情况返回0
	))	
}
