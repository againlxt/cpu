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

class WBU extends Module {
	val io = IO(new Bundle {
		val exu2WBU 	= Flipped(Decoupled(new EXU2WBU))
        val wbu2CSR     = new WBU2CSR
        val wbu2BaseReg = new WBU2BaseReg
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

	/*
	// Data Memory
	val dataMem 		= Module(new DataMem)
	// Input
	dataMem.io.addr 		:= aluDataWire
	dataMem.io.memOP 		:= memOPWire
	dataMem.io.dataIn 		:= memDataWire
	dataMem.io.wrEn 		:= memWRWire
	dataMem.io.valid 		:= memValidWire
	// Output
	val rDataWire 	= dataMem.io.dataOut
	val rValidWire 		= dataMem.io.rValid
	val wValidWire 		= dataMem.io.wValid
	*/	

	/* Data Memory */
	val dataSramAXILite					= Module(new AXILiteSram)
	/* Clock And Reset */
	dataSramAXILite.io.axiLiteM.aclk	:= this.clock.asBool
	dataSramAXILite.io.axiLiteM.aresetn	:= resetnWire
	/* AR */
	dataSramAXILite.io.axiLiteM.arAddr	:= aluDataWire
	val arValidReg 						= RegInit(0.U(1.W))
	dataSramAXILite.io.axiLiteM.arValid	:= arValidReg
	val arReadyWire 					= dataSramAXILite.io.axiLiteM.arReady
	/* R */
	val rDataWire 						= dataSramAXILite.io.axiLiteM.rData
	val signDataWire					= MuxCase(rDataWire.asSInt, Seq(
		(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, rDataWire(7)), rDataWire(7, 0)).asSInt,
		(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, rDataWire(15)), rDataWire(15, 0)).asSInt,
		(wMaskWire === "b1111".U).asBool 	-> rDataWire.asSInt
	))
	val memRdDataWire 					= Mux(sOrUWire.asBool, signDataWire.asUInt, rDataWire)
	val rrEspWire 						= dataSramAXILite.io.axiLiteM.rrEsp
	val rValidWire  					= dataSramAXILite.io.axiLiteM.rValid
	val rReadyReg 						= RegInit(1.U(1.W))
	dataSramAXILite.io.axiLiteM.rReady	:= rReadyReg
	/* AW */
	dataSramAXILite.io.axiLiteM.awAddr	:= aluDataWire
	val awValidReg 						= RegInit(0.U(1.W))
	dataSramAXILite.io.axiLiteM.awValid	:= awValidReg
	val awReadyWire						= dataSramAXILite.io.axiLiteM.awReady
	/* W */
	dataSramAXILite.io.axiLiteM.wData	:= memDataWire
	dataSramAXILite.io.axiLiteM.wStrb	:= wMaskWire
	val wValidReg 						= RegInit(0.U(1.W))
	dataSramAXILite.io.axiLiteM.wValid	:= wValidReg
	val wReadyWire 						= dataSramAXILite.io.axiLiteM.wReady
	/* B */
	val bRespWire						= dataSramAXILite.io.axiLiteM.bResp
	val bValidWire						= dataSramAXILite.io.axiLiteM.bValid
	val bReadyReg						= RegInit(1.U(1.W))
	dataSramAXILite.io.axiLiteM.bReady	:= bReadyReg
	/* Data Memory Headshake */
	/* AR */
	when(~resetnWire.asBool) {
		arValidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool) {
		arValidReg	:= 1.U
	} .elsewhen(arValidReg.asBool && arReadyWire.asBool) {
		arValidReg	:= 0.U
	}
	/* R */
	when(~resetnWire.asBool) {
		rReadyReg	:= 1.U(1.W)
	} .elsewhen(rValidWire.asBool && rReadyReg.asBool && io.exu2WBU.bits.memValid.asBool) {
		rReadyReg	:= 0.U(1.W)
	} .elsewhen(rValidWire.asBool) {
		rReadyReg	:= 1.U(1.W)
	}
	/* AW */
	when(~resetnWire.asBool) {
		awValidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool && io.exu2WBU.bits.memWR.asBool) {
		awValidReg	:= 1.U
	} .elsewhen(awReadyWire.asBool && awValidReg.asBool) {
		awValidReg	:= 0.U
	}
	/* W */
	when(~resetnWire.asBool) {
		wValidReg	:= 0.U
	} .elsewhen(io.exu2WBU.ready && io.exu2WBU.valid && io.exu2WBU.bits.memValid.asBool && io.exu2WBU.bits.memWR.asBool) {
		wValidReg	:= 1.U
	} .elsewhen(wReadyWire.asBool && wValidReg.asBool) {
		wValidReg	:= 0.U
	}
	/* B */
	when(~resetnWire.asBool) {
		bReadyReg	:= 1.U
	} .elsewhen(bValidWire.asBool && bReadyReg.asBool) {
		bReadyReg	:= 0.U
	} .elsewhen(bValidWire.asBool) {
		bReadyReg	:= 1.U
	}
	/* Data Memory End */

	// State Machine
	val s_idle :: s_wait_exu_valid :: s_sram_op :: s_wait_pcReg_ready :: Nil = Enum(4)
	val state = RegInit(s_idle)
	val memEnd = (wReadyWire.asBool && wValidReg.asBool) || (rReadyReg.asBool && rValidWire.asBool)
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
		(toRegWire === "b01".U).asBool -> memRdDataWire,
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

class DataMem extends Module {
	val io = IO(new Bundle{
		// Input
		val addr 	= Input(UInt(32.W))
		val memOP 	= Input(UInt(3.W))
		val dataIn 	= Input(UInt(32.W))
		val wrEn 	= Input(Bool())
		val valid  	= Input(Bool())

		val dataOut = Output(UInt(32.W))
		val rValid 	= Output(UInt(1.W))
		val wValid 	= Output(UInt(1.W))
	})
	val addrWire 	= io.addr
	val memOPWire 	= io.memOP
	val dataInWire 	= io.dataIn
	val wrEnWire 	= io.wrEn
	val validWire 	= io.valid

	val wMaskWire 	= MuxCase (1.U(8.W), Seq(
		(memOPWire === "b000".U).asBool -> "b00000001".U,
		(memOPWire === "b001".U).asBool -> "b00000011".U,
		(memOPWire === "b010".U).asBool -> "b00001111".U,
		(memOPWire === "b101".U).asBool -> "b00000011".U,
		(memOPWire === "b100".U).asBool -> "b00000001".U
	))
	val sOrUWire 	= MuxCase (0.U(1.W), Seq(
		(memOPWire === "b000".U).asBool -> 1.U(1.W),
		(memOPWire === "b001".U).asBool -> 1.U(1.W),
		(memOPWire === "b010".U).asBool -> 1.U(1.W),
		(memOPWire === "b101".U).asBool -> 0.U(1.W),
		(memOPWire === "b100".U).asBool -> 0.U(1.W)
	))

	val dataSRAM 		= Module(new DataSRAM)
	dataSRAM.io.wbuSRAM.clk 	:= this.clock.asUInt
	dataSRAM.io.wbuSRAM.raddr 	:= addrWire
	dataSRAM.io.wbuSRAM.ren 	:= validWire
	val sramData 				= dataSRAM.io.wbuSRAM.rdata
	val rValidWire				= dataSRAM.io.wbuSRAM.rValid
	val wValidWire 				= dataSRAM.io.wbuSRAM.wValid
	dataSRAM.io.wbuSRAM.waddr 	:= addrWire
	dataSRAM.io.wbuSRAM.wdata 	:= dataInWire
	dataSRAM.io.wbuSRAM.wen 	:= validWire && wrEnWire
	dataSRAM.io.wbuSRAM.wmask 	:= wMaskWire(3,0)
	val signdataWire 			= Wire(SInt(32.W))
	signdataWire := MuxCase(sramData.asSInt, Seq(
		(wMaskWire === "b0001".U).asBool 	-> Cat(Fill(24, sramData(7)), sramData(7, 0)).asSInt,
		(wMaskWire === "b0011".U).asBool 	-> Cat(Fill(16, sramData(15)), sramData(15, 0)).asSInt,
		(wMaskWire === "b1111".U).asBool 	-> sramData.asSInt
	))

	io.dataOut 			:= Mux(sOrUWire.asBool, signdataWire.asUInt, sramData)
	io.rValid 			:= rValidWire
	io.wValid 			:= wValidWire
}

class DataSRAM extends Module {
	val io = IO(new Bundle {
		val wbuSRAM = Flipped(new WBUSRAM)
	})

	val dataSRAMV = Module(new DataSRAMV)
	dataSRAMV.io.clk 	:= io.wbuSRAM.clk
	dataSRAMV.io.raddr 	:= io.wbuSRAM.raddr
	dataSRAMV.io.ren 	:= io.wbuSRAM.ren
	io.wbuSRAM.rdata 	:= dataSRAMV.io.rdata
	io.wbuSRAM.rValid 	:= dataSRAMV.io.rValid
	dataSRAMV.io.waddr 	:= io.wbuSRAM.waddr
	dataSRAMV.io.wdata 	:= io.wbuSRAM.wdata
	dataSRAMV.io.wen 	:= io.wbuSRAM.wen
	dataSRAMV.io.wmask 	:= io.wbuSRAM.wmask
	io.wbuSRAM.wValid 	:= dataSRAMV.io.wValid
}

class DataSRAMV extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle {
		val clk 	= Input(UInt(1.W))
		val raddr	= Input(UInt(32.W))
		val ren		= Input(UInt(1.W))
		val rdata	= Output(UInt(32.W))
		val rValid 	= Output(UInt(1.W))
		val waddr	= Input(UInt(32.W))
		val wdata 	= Input(UInt(32.W))
		val wen 	= Input(UInt(1.W))
		val wmask 	= Input(UInt(4.W))
		val wValid 	= Output(UInt(1.W))
	})

	setInline("DataSRAMV.sv",
	"""module DataSRAMV(
	   |	input 		 clk,
	   |	input [31:0] raddr,
	   |	input 		 ren,
	   |	output[31:0] rdata,
	   |	output 		 rValid,
	   |	input [31:0] waddr,
	   |	input [31:0] wdata,
	   |	input 		 wen,
	   |	input [3:0]  wmask,
	   |	output 		 wValid
	   |);
	   |
	   |import "DPI-C" function int unsigned pmem_read(input int unsigned raddr, input byte rmask);
	   |import "DPI-C" function void pmem_write(
	   |	input int unsigned waddr, input int unsigned wdata, input byte wmask);
	   |
	   |reg[31:0] rdataReg;
	   |reg 	  rValidReg;
	   |reg 	  wValidReg;
	   |wire[7:0] wmaskWire;
	   |assign wmaskWire = {4'b0, wmask};
	   |// Memory Read
	   |assign rdata = rdataReg;
	   |assign rValid= rValidReg;
	   |assign wValid= wValidReg;
	   |always@(posedge clk) begin
	   |	if(ren) begin
	   |		rdataReg <= pmem_read(raddr, wmaskWire);
	   |		rValidReg<= 1'b1;
	   |	end
	   |	else begin
	   | 		rdataReg <= rdataReg;
	   |		rValidReg<= 1'b0;
	   |	end
	   |end
	   |
	   |// Memory Write
	   |always@(posedge clk) begin
	   |	if(wen) begin
	   |		pmem_write(waddr, wdata, wmaskWire);
	   |		wValidReg	<= 1'b1;
	   |	end
	   |	else begin
	   |		wValidReg	<= 1'b0;
	   |	end
	   |end
	   |
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
