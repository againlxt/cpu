package wbu

import chisel3._
import chisel3.util._
import memory._
import _root_.interface.EXU2WBU
import _root_.interface.WBU2CSR
import _root_.interface.WBU2BaseReg
import _root_.interface.WBU2PC
import _root_.interface._
import dataclass.data
import basemode.LFSR

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

    val pcReg 			= RegInit(BigInt("80000000", 16).U(32.W))
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
	val awreadyWire 			= io.wbu2Mem.awready
	val awvalidReg 				= RegInit(0.B)
	io.wbu2Mem.awvalid 	:= awvalidReg
	io.wbu2Mem.awaddr	:= aluDataWire
	val awidReg 				= RegInit(0.U(4.W))
	io.wbu2Mem.awid 		:= awidReg
	val awlenReg 				= RegInit(0.U(8.W))
	io.wbu2Mem.awlen 	:= awlenReg
	val awsizeReg 				= RegInit(2.U(3.W))
	io.wbu2Mem.awsize 	:= awsizeReg
	val awburstReg 				= RegInit(1.U(2.W))
	io.wbu2Mem.awburst 	:= awburstReg
	/* W */
	val wreadyWire 				= io.wbu2Mem.wready
	val wvalidReg 				= RegInit(0.B)
	io.wbu2Mem.wvalid 	:= wvalidReg
	io.wbu2Mem.wdata 	:= memDataWire
	io.wbu2Mem.wstrb 	:= wMaskWire
	val wlastReg 				= RegInit(0.B)
	io.wbu2Mem.wlast 	:= wlastReg
	/* B */
	val breadyReg 				= RegInit(1.B)
	io.wbu2Mem.bready 	:= breadyReg
	val bvalidWire 				= io.wbu2Mem.bvalid
	val brespWire 				= io.wbu2Mem.bresp
	val bidWire 				= io.wbu2Mem.bid
	/* AR */
	val arreadyWire 			= io.wbu2Mem.arready
	val arvalidReg 				= RegInit(0.B)
	io.wbu2Mem.arvalid 	:= arvalidReg
	io.wbu2Mem.araddr	:= aluDataWire
	val aridReg 				= RegInit(0.U(4.W))
	io.wbu2Mem.arid 		:= aridReg
	val arlenReg 				= RegInit(0.U(8.W))
	io.wbu2Mem.arlen 	:= arlenReg
	val arsizeReg 				= RegInit(2.U(3.W))
	io.wbu2Mem.arsize 	:= arsizeReg
	val arburstReg 				= RegInit(1.U(2.W))
	io.wbu2Mem.arburst 	:= arburstReg
	/* R */
	val rreadyReg 				= RegInit(1.B)
	io.wbu2Mem.rready 	:= rreadyReg
	val rvalidWire 				= io.wbu2Mem.rvalid
	val rrespWire 				= io.wbu2Mem.rresp
	val rdataWire 				= io.wbu2Mem.rdata
	val signDataWire					= MuxCase(rdataWire.asSInt, Seq(
		(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, rdataWire(7)), rdataWire(7, 0)).asSInt,
		(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, rdataWire(15)), rdataWire(15, 0)).asSInt,
		(wMaskWire === "b1111".U).asBool 	-> rdataWire.asSInt
	))
	val memRdDataReg					= RegInit(0.U(32.W))
	val memRdDataWire 					= Mux(sOrUWire.asBool, signDataWire.asUInt, rdataWire)
	val rlastWire 				= io.wbu2Mem.rlast
	val ridWire 				= io.wbu2Mem.rid
	when(~resetnWire.asBool) {
		memRdDataReg	:= 0.U(32.W)
	} .elsewhen(rvalidWire && io.wbu2Mem.rready) {
		memRdDataReg	:= memRdDataWire
	}
	/* Data Memory Headshake */
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
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool && io.exu2WBU.bits.memWR.asBool) {
		wvalidReg	:= 1.U
	} .elsewhen(wreadyWire && io.wbu2Mem.wvalid) {
		wvalidReg	:= 0.U
	}
	/* B */
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
	/* AXI Transport End */

	/*
	/* AR */
	io.wbu2Mem.arAddr	:= aluDataWire
	val arValidReg 						= RegInit(0.U(1.W))
	io.wbu2Mem.arValid	:= arValidReg
	val arReadyWire 					= io.wbu2Mem.arReady
	/* R */
	val rDataWire 						= io.wbu2Mem.rData
	val signDataWire					= MuxCase(rDataWire.asSInt, Seq(
		(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, rDataWire(7)), rDataWire(7, 0)).asSInt,
		(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, rDataWire(15)), rDataWire(15, 0)).asSInt,
		(wMaskWire === "b1111".U).asBool 	-> rDataWire.asSInt
	))
	val memRdDataReg					= RegInit(0.U(32.W))
	val memRdDataWire 					= Mux(sOrUWire.asBool, signDataWire.asUInt, rDataWire)	
	val rrEspWire 						= io.wbu2Mem.rrEsp
	val rValidWire  					= io.wbu2Mem.rValid
	val rReadyReg 						= RegInit(1.U(1.W))
	io.wbu2Mem.rReady	:= rReadyReg
	when(~resetnWire.asBool) {
		memRdDataReg	:= 0.U(32.W)
	} .elsewhen(rValidWire.asBool && io.wbu2Mem.rReady.asBool) {
		memRdDataReg	:= memRdDataWire
	}
	/* AW */
	io.wbu2Mem.awAddr	:= aluDataWire
	val awValidReg 						= RegInit(0.U(1.W))
	io.wbu2Mem.awValid	:= awValidReg
	val awReadyWire						= io.wbu2Mem.awReady
	/* W */
	io.wbu2Mem.wData	:= memDataWire
	io.wbu2Mem.wStrb	:= wMaskWire
	val wValidReg 						= RegInit(0.U(1.W))
	io.wbu2Mem.wValid	:= wValidReg
	val wReadyWire 						= io.wbu2Mem.wReady
	/* B */
	val bRespWire						= io.wbu2Mem.bResp
	val bValidWire						= io.wbu2Mem.bValid
	val bReadyReg						= RegInit(1.U(1.W))
	io.wbu2Mem.bReady	:= bReadyReg
	/* Data Memory Headshake */
	/* AR */
	when(~resetnWire.asBool) {
		arValidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && (io.exu2WBU.bits.memValid.asBool && (~io.exu2WBU.bits.memWR.asBool))) {
		arValidReg	:= 1.U
	} .elsewhen(io.wbu2Mem.arValid.asBool && io.wbu2Mem.arReady.asBool) {
		arValidReg	:= 0.U
	}
	/* R */
	when(~resetnWire.asBool) {
		rReadyReg	:= 1.U(1.W)
	} .elsewhen(rValidWire.asBool && io.wbu2Mem.rReady.asBool && io.exu2WBU.bits.memValid.asBool) {
		rReadyReg		:= 0.U(1.W) 
	} .elsewhen(rValidWire.asBool) {
		rReadyReg	:= 1.U(1.W)
	}
	/* AW */
	when(~resetnWire.asBool) {
		awValidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool && io.exu2WBU.bits.memWR.asBool) {
		awValidReg	:= 1.U
	} .elsewhen(awReadyWire.asBool && io.wbu2Mem.awValid.asBool) {
		awValidReg	:= 0.U
	}
	/* W */
	when(~resetnWire.asBool) {
		wValidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool && io.exu2WBU.bits.memWR.asBool) {
		wValidReg	:= 1.U
	} .elsewhen(wReadyWire.asBool && io.wbu2Mem.wValid.asBool) {
		wValidReg	:= 0.U
	}
	/* B */
	when(~resetnWire.asBool) {
		bReadyReg	:= 1.U
	} .elsewhen(bValidWire.asBool && io.wbu2Mem.bReady.asBool) {
		bReadyReg	:= 0.U
	} .elsewhen(bValidWire.asBool) {
		bReadyReg	:= 1.U
	}
	/* Data Memory End */
	*/

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

	// Output
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
