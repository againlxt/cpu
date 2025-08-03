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

    val pcReg 			= RegInit(Mux(Config.SoC.asBool, "h30000000".U(32.W), "h80000000".U(32.W))) 
	val memDataReg		= RegInit(0.U(32.W))
	val aluDataReg		= RegInit(0.U(32.W))
	val csrWDataReg		= RegInit(0.U(32.W))
    val csrDataReg		= RegInit(0.U(32.W))
	val immDataReg 	    = RegInit(0.U(32.W))
	val rs1DataReg 	    = RegInit(0.U(32.W))
	val instReg 		= RegInit(0.U(32.W))

    val regWRReg        = RegInit(0.U(1.W))
	val memWRReg 		= RegInit(0.U(1.W))
	val memValidReg	    = RegInit(0.U(1.W))
	val memOPReg 		= RegInit(0.U(3.W))
	val toRegReg 		= RegInit(0.U(2.W))
	val ecallReg 		= RegInit(0.U(1.W))
	val csrEnReg 		= RegInit(0.U(1.W))
	val csrWrReg 		= RegInit(0.U(1.W))
	val wbu2IcacheReg	= RegInit(0.U(1.W))

    // Data signal storage
	when(io.exu2LSU.ready && io.exu2LSU.valid) {
        pcReg 			:= io.exu2LSU.bits.pc
        memDataReg		:= io.exu2LSU.bits.memData
        aluDataReg		:= io.exu2LSU.bits.aluData
        csrWDataReg		:= io.exu2LSU.bits.csrWData
        csrDataReg		:= io.exu2LSU.bits.csrData
        immDataReg 	    := io.exu2LSU.bits.immData
        rs1DataReg 	    := io.exu2LSU.bits.rs1Data
        instReg 		:= io.exu2LSU.bits.inst
        regWRReg        := io.exu2LSU.bits.regWR
        memWRReg 		:= io.exu2LSU.bits.memWR
        memValidReg	    := io.exu2LSU.bits.memValid
        memOPReg 		:= io.exu2LSU.bits.memOP
        toRegReg 		:= io.exu2LSU.bits.toReg
        ecallReg 		:= io.exu2LSU.bits.ecall
        csrEnReg 		:= io.exu2LSU.bits.csrEn
        csrWrReg 		:= io.exu2LSU.bits.csrWr
    }
    val wMaskWire 	= MuxCase (1.U(4.W), Seq(
		(memOPReg(1,0) === 0.U).asBool -> "b0001".U,
		(memOPReg(1,0) === 1.U).asBool -> "b0011".U,
		(memOPReg(1,0) === 2.U).asBool -> "b1111".U
	))
	val sOrUWire 	= MuxCase (0.U(1.W), Seq(
		(memOPReg(2) === 0.U).asBool -> 1.U(1.W),
		(memOPReg(2) === 1.U).asBool -> 0.U(1.W)
	))

    /* AXI Transport */
	/* AW */
	val awreadyWire 	= io.lsu2Mem.awready
	val awvalidReg 		= RegInit(0.B)
	io.lsu2Mem.awvalid 	:= awvalidReg
	io.lsu2Mem.awaddr	:= aluDataReg
	val awidReg 		= RegInit(0.U(4.W))
	io.lsu2Mem.awid 	:= awidReg
	val awlenReg 		= RegInit(0.U(8.W))
	io.lsu2Mem.awlen 	:= awlenReg
	io.lsu2Mem.awsize 	:= memOPReg(1,0)
	val awburstReg 		= RegInit(1.U(2.W))
	io.lsu2Mem.awburst 	:= awburstReg
	/* W */
	val wreadyWire 		= io.lsu2Mem.wready
	val wvalidReg 		= RegInit(0.B)
	io.lsu2Mem.wvalid 	:= wvalidReg
	if (Config.SoC) {
		io.lsu2Mem.wdata 	:= MuxCase(memDataReg, Seq(
			(aluDataReg(1,0) === 0.U && memOPReg(1,0) === 2.U).asBool	-> memDataReg,
			(aluDataReg(1,0) === 0.U && memOPReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), memDataReg(15,0)),
			(aluDataReg(1,0) === 0.U && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(24.W), memDataReg(7,0)),
			(aluDataReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), Cat(memDataReg(7,0), 0.U(8.W))),
			(aluDataReg(1,0) === 2.U && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(8.W), Cat(memDataReg(7,0), 0.U(16.W))),
			(aluDataReg(1,0) === 2.U && memOPReg(1,0) === 1.U).asBool	-> Cat(memDataReg(15,0), 0.U(16.W)),
			(aluDataReg(1,0) === 3.U).asBool	-> Cat(memDataReg(7,0), 0.U(24.W))
		))
		io.lsu2Mem.wstrb 	:= MuxCase(wMaskWire, Seq(
			(aluDataReg(1,0) === 0.U & memWRReg.asBool).asBool	-> wMaskWire,
			(aluDataReg(1,0) === 1.U & memWRReg.asBool).asBool	-> Cat(wMaskWire(2,0), 0.U(1.W)),
			(aluDataReg(1,0) === 2.U & memWRReg.asBool).asBool	-> Cat(wMaskWire(1,0), 0.U(2.W)),
			(aluDataReg(1,0) === 3.U & memWRReg.asBool).asBool	-> Cat(wMaskWire(0), 0.U(3.W))
		))
	} else {
		io.lsu2Mem.wdata	:= memDataReg
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
	io.lsu2Mem.araddr	:= aluDataReg
	val aridReg 		= RegInit(0.U(4.W))
	io.lsu2Mem.arid 	:= aridReg
	val arlenReg 		= RegInit(0.U(8.W))
	io.lsu2Mem.arlen 	:= arlenReg
	io.lsu2Mem.arsize 	:= memOPReg(1,0) 
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
			((aluDataReg <= "h20000fff".U) && (aluDataReg >= "h20000000".U) && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataReg <= "h20000fff".U) && (aluDataReg >= "h20000000".U) && memOPReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataReg <= "h20000fff".U) && (aluDataReg >= "h20000000".U) && memOPReg(1,0) === 2.U).asBool	-> rdataReg,

			/* FLASH Read */
			((aluDataReg <= "h3fffffff".U) && (aluDataReg >= "h30000000".U) && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataReg <= "h3fffffff".U) && (aluDataReg >= "h30000000".U) && memOPReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataReg <= "h3fffffff".U) && (aluDataReg >= "h30000000".U) && memOPReg(1,0) === 2.U).asBool	-> rdataReg,

			/* PSRAM Read */
			((aluDataReg >= "h80000000".U) && aluDataReg(1,0) === 0.U && memOPReg(1,0) === 2.U).asBool -> rdataReg,
			((aluDataReg >= "h80000000".U) && aluDataReg(1,0) === 0.U && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataReg >= "h80000000".U) && aluDataReg(1,0) === 0.U && memOPReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataReg >= "h80000000".U) && aluDataReg(1,0) === 1.U).asBool	-> Cat(0.U(24.W), rdataReg(15,8)),
			((aluDataReg >= "h80000000".U) && aluDataReg(1,0) === 2.U && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(23,16)),
			((aluDataReg >= "h80000000".U) && aluDataReg(1,0) === 2.U && memOPReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(31,16)),
			((aluDataReg >= "h80000000".U) && aluDataReg(1,0) === 3.U).asBool	-> Cat(0.U(24.W), rdataReg(31,24)),
			
			/* SRAM Read */
			((aluDataReg <= "h0f001fff".U) && (aluDataReg >= "h0f00000".U) && aluDataReg(1,0) === 0.U && memOPReg(1,0) === 2.U).asBool -> rdataReg,
			((aluDataReg <= "h0f001fff".U) && (aluDataReg >= "h0f00000".U) && aluDataReg(1,0) === 0.U && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(7,0)),
			((aluDataReg <= "h0f001fff".U) && (aluDataReg >= "h0f00000".U) && aluDataReg(1,0) === 0.U && memOPReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(15,0)),
			((aluDataReg <= "h0f001fff".U) && (aluDataReg >= "h0f00000".U) && aluDataReg(1,0) === 1.U).asBool	-> Cat(0.U(24.W), rdataReg(15,8)),
			((aluDataReg <= "h0f001fff".U) && (aluDataReg >= "h0f00000".U) && aluDataReg(1,0) === 2.U && memOPReg(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataReg(23,16)),
			((aluDataReg <= "h0f001fff".U) && (aluDataReg >= "h0f00000".U) && aluDataReg(1,0) === 2.U && memOPReg(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataReg(31,16)),
			((aluDataReg <= "h0f001fff".U) && (aluDataReg >= "h0f00000".U) && aluDataReg(1,0) === 3.U).asBool	-> Cat(0.U(24.W), rdataReg(31,24)),
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
        Mux(io.exu2LSU.bits.memWR.asBool, s_write, s_read), s_wait_ready), s_wait_valid),
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
		getCmd.io.cmd 		:= instReg
	}
	if(Config.hasDPIC & (!Config.isSTA)) {
		val mTrace 			= Module(new MTrace)
		mTrace.io.data 		:= Mux(memWRReg.asBool, io.lsu2Mem.wdata, Mux(sOrUWire.asBool, signDataWire.asUInt, rdataShiftWire))
		mTrace.io.addr 		:= aluDataReg
		mTrace.io.memop 	:= memOPReg(1,0)
		mTrace.io.wOrR 		:= memWRReg.asBool
		mTrace.io.enable	:= memEnd
	}

    /* IO */
    io.exu2LSU.ready            := (state === s_wait_valid)
    io.lsu2WBU.valid            := (state === s_wait_ready)
    io.lsu2WBU.bits.pc          := pcReg
    io.lsu2WBU.bits.memData     := memRdDataWire
    io.lsu2WBU.bits.aluData     := aluDataReg
    io.lsu2WBU.bits.csrWData    := csrWDataReg
    io.lsu2WBU.bits.csrData     := csrDataReg
    io.lsu2WBU.bits.rs1Data     := rs1DataReg
    io.lsu2WBU.bits.inst        := instReg
    io.lsu2WBU.bits.regWR       := regWRReg
    io.lsu2WBU.bits.toReg       := toRegReg
    io.lsu2WBU.bits.ecall       := ecallReg
    io.lsu2WBU.bits.csrEn       := csrEnReg
    io.lsu2WBU.bits.csrWr       := csrWrReg
    io.lsu2WBU.bits.fencei      := (memOPReg === 7.U)
	io.lsu2BaseReg.rdIndex 		:= instReg(11,7)
	io.lsu2BaseReg.regWR 		:= regWRReg
	io.lsu2BaseReg.pc			:= pcReg
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
