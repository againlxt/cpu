package wbu

import chisel3._
import chisel3.util._
import memory._
import _root_.interface.EXU2WBU
import _root_.interface.WBU2CSR
import _root_.interface.WBU2BaseReg
import _root_.interface.WBU2PC
import _root_.interface._
import basemode.AXIAccessFault
import dataclass.data
import basemode.LFSR
import cpu.Config
import dpic._

class WBU extends Module {
	val io = IO(new Bundle {
		val exu2WBU 	= Flipped(Decoupled(new EXU2WBU))	
        val wbu2CSR     = new WBU2CSR
        val wbu2BaseReg = new WBU2BaseReg
		val wbu2Mem 	= new AXI
        val wbu2PC      = Decoupled(new WBU2PC)
	})
	val clockWire 		= this.clock.asBool
	val resetnWire		= ~this.reset.asBool

    val pcReg 			= RegInit(BigInt("20000000", 16).U(32.W))
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
	val branchCtrReg 	= RegInit(0.U(4.W))
	val lessReg 		= RegInit(0.U(1.W))
	val zeroReg 		= RegInit(0.U(1.W))
	val ecallReg 		= RegInit(0.U(1.W))
	val csrEnReg 		= RegInit(0.U(1.W))
	val csrWrReg 		= RegInit(0.U(1.W))

    val pcWire          = pcReg
    val memDataWire     = memDataReg
    val aluDataWire     = aluDataReg
    val csrWDataWire    = csrWDataReg
    val csrDataWire     = csrDataReg
    val immDataWire     = immDataReg
    val instWire        = instReg
    val rs1DataWire     = rs1DataReg
    val regWRWire       = regWRReg
    val memWRWire       = memWRReg
    val memValidWire    = memValidReg
    val memOPWire       = memOPReg
    val toRegWire       = toRegReg
    val branchCtrWire   = branchCtrReg
    val lessWire        = lessReg
    val zeroWire        = zeroReg
    val ecallWire       = ecallReg
    val csrEnWire       = csrEnReg
    val csrWrWire       = csrWrReg

    val ready2EXUReg= RegInit(1.U(1.W))
    io.exu2WBU.ready   := ready2EXUReg.asBool
	val validPC2Reg	= RegInit(0.U(1.W))
	io.wbu2PC.valid    := validPC2Reg.asBool 

    // Data signal storage
	when(io.exu2WBU.ready && io.exu2WBU.valid) {
        pcReg 			:= io.exu2WBU.bits.pc
        memDataReg		:= io.exu2WBU.bits.memData
        aluDataReg		:= io.exu2WBU.bits.aluData
        csrWDataReg		:= io.exu2WBU.bits.csrWData
        csrDataReg		:= io.exu2WBU.bits.csrData
        immDataReg 	    := io.exu2WBU.bits.immData
        rs1DataReg 	    := io.exu2WBU.bits.rs1Data
        instReg 		:= io.exu2WBU.bits.inst
        regWRReg        := io.exu2WBU.bits.regWR
        memWRReg 		:= io.exu2WBU.bits.memWR
        memValidReg	    := io.exu2WBU.bits.memValid
        memOPReg 		:= io.exu2WBU.bits.memOP
        toRegReg 		:= io.exu2WBU.bits.toReg
        branchCtrReg 	:= io.exu2WBU.bits.branchCtr
        lessReg 		:= io.exu2WBU.bits.less
        zeroReg 		:= io.exu2WBU.bits.zero
        ecallReg 		:= io.exu2WBU.bits.ecall
        csrEnReg 		:= io.exu2WBU.bits.csrEn
        csrWrReg 		:= io.exu2WBU.bits.csrWr
    }
	val wMaskWire 	= MuxCase (1.U(4.W), Seq(
		(memOPWire === "b000".U).asBool -> "b0001".U,
		(memOPWire === "b001".U).asBool -> "b0011".U,
		(memOPWire === "b010".U).asBool -> "b1111".U,
		(memOPWire === "b101".U).asBool -> "b0011".U,
		(memOPWire === "b100".U).asBool -> "b0001".U
	))
	val sOrUWire 	= MuxCase (0.U(1.W), Seq(
		(memOPWire === "b000".U).asBool -> 1.U(1.W),
		(memOPWire === "b001".U).asBool -> 1.U(1.W),
		(memOPWire === "b010".U).asBool -> 1.U(1.W),
		(memOPWire === "b101".U).asBool -> 0.U(1.W),
		(memOPWire === "b100".U).asBool -> 0.U(1.W)
	))

	// Branch Cond
	val branchCond 		= Module(new BranchCond)
	// Input
	branchCond.io.branch:= branchCtrWire
	branchCond.io.less 	:= lessWire
	branchCond.io.zero 	:= zeroWire
	// Output
	val pcASrcWire 		= branchCond.io.pcASrc
	val pcBSrcWire 		= branchCond.io.pcBSrc

	/* AXI Transport */
	/* AW */
	val awreadyWire 	= io.wbu2Mem.awready
	val awvalidReg 		= RegInit(0.B)
	io.wbu2Mem.awvalid 	:= awvalidReg
	io.wbu2Mem.awaddr	:= aluDataWire
	val awidReg 		= RegInit(0.U(4.W))
	io.wbu2Mem.awid 	:= awidReg
	val awlenReg 		= RegInit(0.U(8.W))
	io.wbu2Mem.awlen 	:= awlenReg
	io.wbu2Mem.awsize 	:= memOPWire(1,0)
	val awburstReg 		= RegInit(1.U(2.W))
	io.wbu2Mem.awburst 	:= awburstReg
	/* W */
	val wreadyWire 		= io.wbu2Mem.wready
	val wvalidReg 		= RegInit(0.B)
	io.wbu2Mem.wvalid 	:= wvalidReg
	if (Config.SoC) {
		io.wbu2Mem.wdata 	:= MuxCase(memDataWire, Seq(
			(aluDataWire(1,0) === 0.U && memOPWire(1,0) === 2.U).asBool	-> memDataWire,
			(aluDataWire(1,0) === 0.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), memDataWire(15,0)),
			(aluDataWire(1,0) === 0.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), memDataWire(7,0)),
			(aluDataWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), Cat(memDataWire(7,0), 0.U(8.W))),
			(aluDataWire(1,0) === 2.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(8.W), Cat(memDataWire(7,0), 0.U(16.W))),
			(aluDataWire(1,0) === 2.U && memOPWire(1,0) === 1.U).asBool	-> Cat(memDataWire(15,0), 0.U(16.W)),
			(aluDataWire(1,0) === 3.U).asBool	-> Cat(memDataWire(7,0), 0.U(24.W))
		))
		io.wbu2Mem.wstrb 	:= MuxCase(wMaskWire, Seq(
			(aluDataWire(1,0) === 0.U & memWRWire.asBool).asBool	-> wMaskWire,
			(aluDataWire(1,0) === 1.U & memWRWire.asBool).asBool	-> Cat(wMaskWire(2,0), 0.U(1.W)),
			(aluDataWire(1,0) === 2.U & memWRWire.asBool).asBool	-> Cat(wMaskWire(1,0), 0.U(2.W)),
			(aluDataWire(1,0) === 3.U & memWRWire.asBool).asBool	-> Cat(wMaskWire(0), 0.U(3.W))
		))
	} else {
		io.wbu2Mem.wdata	:= memDataWire
		io.wbu2Mem.wstrb	:= wMaskWire
	}
	val wlastReg 		= RegInit(0.B)
	io.wbu2Mem.wlast 	:= wlastReg
	/* B */
	val breadyReg 		= RegInit(1.B)
	io.wbu2Mem.bready 	:= breadyReg
	val bvalidWire 		= io.wbu2Mem.bvalid
	val brespWire 		= io.wbu2Mem.bresp
	val bidWire 		= io.wbu2Mem.bid
	/* AR */
	val arreadyWire 	= io.wbu2Mem.arready
	val arvalidReg 		= RegInit(0.B)
	io.wbu2Mem.arvalid 	:= arvalidReg
	io.wbu2Mem.araddr	:= aluDataWire
	val aridReg 		= RegInit(0.U(4.W))
	io.wbu2Mem.arid 	:= aridReg
	val arlenReg 		= RegInit(0.U(8.W))
	io.wbu2Mem.arlen 	:= arlenReg
	io.wbu2Mem.arsize 	:= memOPWire(1,0) 
	val arburstReg 		= RegInit(1.U(2.W))
	io.wbu2Mem.arburst 	:= arburstReg
	/* R */
	val rreadyReg 		= RegInit(1.B)
	io.wbu2Mem.rready 	:= rreadyReg
	val rvalidWire 		= io.wbu2Mem.rvalid
	val rrespWire 		= io.wbu2Mem.rresp
	val rdataWire 		= io.wbu2Mem.rdata
	val rdataShiftWire  = Wire(UInt(32.W))
	val signDataWire 	= Wire(SInt(32.W))
	if(Config.SoC) {
		rdataShiftWire 	:= MuxCase(rdataWire, Seq(
			/* MROM Read */
			((aluDataWire <= "h20000fff".U) && (aluDataWire >= "h20000000".U) && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataWire(7,0)),
			((aluDataWire <= "h20000fff".U) && (aluDataWire >= "h20000000".U) && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataWire(15,0)),
			((aluDataWire <= "h20000fff".U) && (aluDataWire >= "h20000000".U) && memOPWire(1,0) === 2.U).asBool	-> rdataWire,

			/* FLASH Read */
			((aluDataWire <= "h3fffffff".U) && (aluDataWire >= "h30000000".U) && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataWire(7,0)),
			((aluDataWire <= "h3fffffff".U) && (aluDataWire >= "h30000000".U) && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataWire(15,0)),
			((aluDataWire <= "h3fffffff".U) && (aluDataWire >= "h30000000".U) && memOPWire(1,0) === 2.U).asBool	-> rdataWire,

			/* PSRAM Read */
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 2.U).asBool -> rdataWire,
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataWire(7,0)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataWire(15,0)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 1.U).asBool	-> Cat(0.U(24.W), rdataWire(15,8)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataWire(23,16)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataWire(31,16)),
			((aluDataWire >= "h80000000".U) && aluDataWire(1,0) === 3.U).asBool	-> Cat(0.U(24.W), rdataWire(31,24)),
			
			/* SRAM Read */
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 2.U).asBool -> rdataWire,
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataWire(7,0)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 0.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataWire(15,0)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 1.U).asBool	-> Cat(0.U(24.W), rdataWire(15,8)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 0.U).asBool	-> Cat(0.U(24.W), rdataWire(23,16)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 2.U && memOPWire(1,0) === 1.U).asBool	-> Cat(0.U(16.W), rdataWire(31,16)),
			((aluDataWire <= "h0f001fff".U) && (aluDataWire >= "h0f00000".U) && aluDataWire(1,0) === 3.U).asBool	-> Cat(0.U(24.W), rdataWire(31,24)),
		))
		signDataWire					:= MuxCase(rdataShiftWire.asSInt, Seq(
			(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, rdataShiftWire(7)), rdataShiftWire(7, 0)).asSInt,
			(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, rdataShiftWire(15)), rdataShiftWire(15, 0)).asSInt,
			(wMaskWire === "b1111".U).asBool 	-> rdataShiftWire.asSInt
		))
	} else {
		rdataShiftWire 	:= MuxCase(rdataWire, Seq(
			(wMaskWire === "b0001".U).asBool 	-> Cat(0.U(24.W), rdataWire(7, 0)),
			(wMaskWire === "b0011".U).asBool 	-> Cat(0.U(16.W), rdataWire(15, 0)),
			(wMaskWire === "b1111".U).asBool 	-> rdataWire
		))
		signDataWire					:= MuxCase(rdataWire.asSInt, Seq(
			(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, rdataWire(7)), rdataWire(7, 0)).asSInt,
			(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, rdataWire(15)), rdataWire(15, 0)).asSInt,
			(wMaskWire === "b1111".U).asBool 	-> rdataWire.asSInt
		))
	}
	val memRdDataReg	= RegInit(0.U(32.W))
	val memRdDataWire 	= Mux(sOrUWire.asBool, signDataWire.asUInt, rdataShiftWire)
	val rlastWire 		= io.wbu2Mem.rlast
	val ridWire 		= io.wbu2Mem.rid
	when(~resetnWire.asBool) {
		memRdDataReg	:= 0.U(32.W)
	} .elsewhen(rvalidWire && io.wbu2Mem.rready) {
		memRdDataReg	:= memRdDataWire
	}
	/* Data Memory Headshake */
	assert(!(io.wbu2Mem.awvalid & (io.wbu2Mem.awaddr <= 0x0F000000.U)) | !(io.wbu2Mem.arvalid & (io.wbu2Mem.araddr <= 0x0F000000.U)));
	/* AW */
	when(~resetnWire.asBool) {
		awvalidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool && io.exu2WBU.bits.memWR.asBool) {
		awvalidReg	:= 1.U
	} .elsewhen(awreadyWire && io.wbu2Mem.awvalid) {
		awvalidReg	:= 0.U
	}
	/* W */
	when(~resetnWire.asBool) {
		wvalidReg	:= 0.U
		wlastReg 	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool && io.exu2WBU.bits.memWR.asBool) {
		wvalidReg	:= 1.U
		wlastReg 	:= 1.U
	} .elsewhen(wreadyWire && io.wbu2Mem.wvalid) {
		wvalidReg	:= 0.U
		wlastReg 	:= 0.U
	}
	/* B */
	if(Config.hasDPIC & (!Config.isSTA)) {
		val axiAccessFault = Module(new AXIAccessFault())
		axiAccessFault.io.ready := breadyReg
		axiAccessFault.io.valid := bvalidWire
		axiAccessFault.io.resp	:= brespWire
	}	
	when(~resetnWire.asBool) {
		breadyReg	:= 1.U
	} .elsewhen(bvalidWire && io.wbu2Mem.bready) {
		breadyReg	:= 0.U
	} .elsewhen(bvalidWire) {
		breadyReg	:= 1.U
	}
	/* AR */
	when(~resetnWire.asBool) {
		arvalidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && (io.exu2WBU.bits.memValid.asBool && (~io.exu2WBU.bits.memWR.asBool))) {
		arvalidReg	:= 1.U
	} .elsewhen(io.wbu2Mem.arvalid && io.wbu2Mem.arready) {
		arvalidReg	:= 0.U
	}
	/* R */
	when(~resetnWire.asBool) {
		rreadyReg	:= 1.U(1.W)
	} .elsewhen(rvalidWire && io.wbu2Mem.rready && io.exu2WBU.bits.memValid.asBool) {
		rreadyReg	:= 0.U(1.W) 
	} .elsewhen(rvalidWire) {
		rreadyReg	:= 1.U(1.W)
	}

	// State Machine
	val s_idle :: s_wait_exu_valid :: s_sram_op :: s_wait_pcReg_ready :: Nil = Enum(4)
	val state = RegInit(s_idle)
	val memEnd = (wreadyWire && wvalidReg) || (rreadyReg && rvalidWire)
	state := MuxLookup(state, s_idle)(List(
		s_idle				-> Mux(reset.asBool, s_idle, s_wait_exu_valid),
		s_wait_exu_valid	-> Mux(reset.asBool, s_idle, Mux(io.exu2WBU.valid, Mux(io.exu2WBU.bits.memValid.asBool, s_sram_op, s_wait_pcReg_ready), s_wait_exu_valid)),
		s_sram_op 			-> Mux(reset.asBool, s_idle, Mux(memEnd, s_wait_pcReg_ready, s_sram_op)),
		s_wait_pcReg_ready	-> Mux(reset.asBool, s_idle, Mux(io.wbu2PC.ready, s_idle, s_wait_pcReg_ready))
	))
	// handshake signals control
	when(state === s_idle) {
		ready2EXUReg 	:= 1.U
		validPC2Reg 	:= 0.U
	} .elsewhen(state === s_wait_exu_valid) {
		ready2EXUReg 	:= 1.U
		validPC2Reg 	:= 0.U
	} .elsewhen(state === s_sram_op) {
		ready2EXUReg 	:= 0.U
		validPC2Reg 	:= 0.U
	} .elsewhen(state === s_wait_pcReg_ready) {
		ready2EXUReg 	:= 0.U
		validPC2Reg 	:= 1.U
	}

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val lsuGetDataCnt = RegInit(0.U(32.W))
		when ((io.wbu2Mem.arvalid && io.wbu2Mem.arready) || (io.wbu2Mem.awready && io.wbu2Mem.awvalid)) {
			lsuGetDataCnt := 0.U
		} .otherwise {
			lsuGetDataCnt := lsuGetDataCnt + 1.U
		}
		val LGDC 			= Module(new PerformanceCounter)
		LGDC.io.valid		:= ((io.wbu2Mem.rvalid && io.wbu2Mem.rready) || (io.wbu2Mem.wready && io.wbu2Mem.wvalid)) && memValidReg.asBool
		LGDC.io.counterType	:= PerformanceCounterType.LSUGETDATA.asUInt
		LGDC.io.data 		:= Mux(((io.wbu2Mem.arvalid && io.wbu2Mem.arready) || (io.wbu2Mem.awready && io.wbu2Mem.awvalid)).asBool, 1.U, lsuGetDataCnt + 1.U)
	}

	/* DPIC */
	if(!Config.isSTA) {
		val getCmd 			= Module(new GetCommond)
		getCmd.io.cmd 		:= instWire
	}
	if(Config.hasDPIC & (!Config.isSTA)) {
		val mTrace 			= Module(new MTrace)
		mTrace.io.data 		:= Mux(memWRReg.asBool, io.wbu2Mem.wdata, Mux(sOrUWire.asBool, signDataWire.asUInt, rdataShiftWire))
		mTrace.io.addr 		:= aluDataWire
		mTrace.io.memop 	:= memOPWire(1,0)
		mTrace.io.wOrR 		:= memWRReg.asBool
		mTrace.io.enable	:= ((wvalidReg & wreadyWire) | (rreadyReg & rvalidWire)) & memValidReg	
	}

	/* Output */
    io.wbu2CSR.pc       := pcWire
    io.wbu2CSR.csrWData := csrWDataWire
    io.wbu2CSR.csr      := instWire(31,20)
    io.wbu2CSR.ecall    := ecallWire
    io.wbu2CSR.csrEn    := csrEnWire
    io.wbu2CSR.csrWr    := csrWrWire

    io.wbu2BaseReg.data := MuxCase(	0.U(32.W), Seq(	
        (toRegWire === "b00".U).asBool -> aluDataWire,
		(toRegWire === "b01".U).asBool -> memRdDataReg,
		(toRegWire === "b10".U).asBool -> csrDataWire
    ))
    io.wbu2BaseReg.rdIndex  := instWire(11,7)
    io.wbu2BaseReg.regWR    := regWRWire

    io.wbu2PC.bits.nextPC   := MuxCase(	0.U(32.W), Seq(	
        (pcASrcWire === "b00".U).asBool	-> 4.U,
		(pcASrcWire === "b01".U).asBool -> immDataWire,
		(pcASrcWire === "b10".U).asBool -> 0.U
    )) + MuxCase(	0.U(32.W), Seq(	
        (pcBSrcWire === "b00".U).asBool	-> pcWire,
		(pcBSrcWire === "b01".U).asBool -> rs1DataWire,
		(pcBSrcWire === "b10".U).asBool -> csrWDataWire
    ))
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

	io.pcASrc 	:= MuxCase (0.U, Seq(
		(branchWire === "b0000".U).asBool -> 0.U,
		(branchWire === "b0001".U).asBool -> 1.U,
		(branchWire === "b0010".U).asBool -> 1.U,
		(branchWire === "b0100".U & !zeroWire).asBool -> 0.U,
		(branchWire === "b0100".U & zeroWire).asBool -> 1.U,
		(branchWire === "b0101".U & !zeroWire).asBool -> 1.U,
		(branchWire === "b0101".U & zeroWire).asBool -> 0.U,
		(branchWire === "b0110".U & !lessWire).asBool -> 0.U,
		(branchWire === "b0110".U & lessWire).asBool -> 1.U,
		(branchWire === "b0111".U & !lessWire).asBool -> 1.U,
		(branchWire === "b0111".U & lessWire).asBool -> 0.U,
		(branchWire === "b1000".U).asBool -> 2.U
	))

	io.pcBSrc 	:= MuxCase (0.U, Seq(
		(branchWire === "b0000".U).asBool -> 0.U,
		(branchWire === "b0001".U).asBool -> 0.U,
		(branchWire === "b0010".U).asBool -> 1.U,
		(branchWire === "b0100".U & !zeroWire).asBool -> 0.U,
		(branchWire === "b0100".U & zeroWire).asBool -> 0.U,
		(branchWire === "b0101".U & !zeroWire).asBool -> 0.U,
		(branchWire === "b0101".U & zeroWire).asBool -> 0.U,
		(branchWire === "b0110".U & !lessWire).asBool -> 0.U,
		(branchWire === "b0110".U & lessWire).asBool -> 0.U,
		(branchWire === "b0111".U & !lessWire).asBool -> 0.U,
		(branchWire === "b1000".U).asBool -> 2.U
	))
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
