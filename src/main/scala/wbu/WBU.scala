package wbu

import chisel3._
import chisel3.util._
import memory._
import _root_.interface.EXU2WBU
import _root_.interface.WBU2CSR
import _root_.interface.WBU2BaseReg
import _root_.interface.WBU2PC

class WBU extends Module {
	val io = IO(new Bundle {
		val exu2WBU 	= Flipped(Decoupled(new EXU2WBU))
        val wbu2CSR     = new WBU2CSR
        val wbu2BaseReg = new WBU2BaseReg
        val wbu2PC      = Decoupled(new WBU2PC)
	})

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

    // handshake signals control
    when(ready2EXUReg === 0.U) {
        ready2EXUReg := 1.U;
    } .otherwise {
        when(io.exu2WBU.valid && io.exu2WBU.ready) {
            ready2EXUReg := 0.U
        }
    }

	when(validPC2Reg === 0.U) {
        when(io.exu2WBU.valid && io.exu2WBU.ready) {
            validPC2Reg := 1.U
        }
    } .otherwise {
        validPC2Reg	:= 0.U
    }

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

	// Branch Cond
	val branchCond 		= Module(new BranchCond)
	// Input
	branchCond.io.branch:= branchCtrWire
	branchCond.io.less 	:= lessWire
	branchCond.io.zero 	:= zeroWire
	// Output
	val pcASrcWire 		= branchCond.io.pcASrc
	val pcBSrcWire 		= branchCond.io.pcBSrc

	// Data Memory
	val dataMem 		= Module(new DataMem)
	// Input
	dataMem.io.addr 		:= aluDataWire
	dataMem.io.memOP 		:= memOPWire
	dataMem.io.dataIn 		:= memDataWire
	dataMem.io.wrEn 		:= memWRWire
	dataMem.io.valid 		:= memValidWire
	// Output
	val dataOutWire 	= dataMem.io.dataOut

	// Output
    io.wbu2CSR.pc       := pcWire
    io.wbu2CSR.csrWData := csrWDataWire
    io.wbu2CSR.csr      := instWire(31,20)
    io.wbu2CSR.ecall    := ecallWire
    io.wbu2CSR.csrEn    := csrEnWire
    io.wbu2CSR.csrWr    := csrWrWire

    io.wbu2BaseReg.data := MuxCase(	0.U(32.W), Seq(	
        (toRegWire === "b00".U).asBool -> aluDataWire,
		(toRegWire === "b01".U).asBool -> dataOutWire,
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
	
	val dataMem 		= Module(new DataMemV())
	dataMem.io.clk 		:= this.clock.asUInt
	dataMem.io.addr 	:= addrWire
	dataMem.io.wmask 	:= wMaskWire
	dataMem.io.sOrU 	:= sOrUWire
	dataMem.io.dataIn 	:= dataInWire
	dataMem.io.wrEn 	:= wrEnWire
	dataMem.io.valid 	:= validWire
	val signdataWire 	= Wire(SInt(32.W))
	val unsigndataWire  = Wire(UInt(32.W))
	signdataWire 		:= dataMem.io.dataOut.asSInt
	unsigndataWire		:= dataMem.io.dataOut

	io.dataOut 			:= Mux(sOrUWire.asBool, signdataWire.asUInt, unsigndataWire)
}

class DataMemV extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle{
		// Input
		val clk 	= Input(UInt(1.W))
		val addr 	= Input(UInt(32.W))
		val wmask 	= Input(UInt(8.W))
		val sOrU 	= Input(Bool())
		val dataIn 	= Input(UInt(32.W))
		val wrEn 	= Input(Bool())
		val valid  	= Input(Bool())

		val dataOut = Output(UInt(32.W))
	})

	setInline("DataMemV.sv",
	"""module DataMemV(
	   |	input 		clk,
	   |	input [31:0] addr,
	   |	input [7:0]  wmask,
	   |	input 		 sOrU,
	   |	input [31:0] dataIn,
	   |	input 		 wrEn,
	   |	input 		 valid,
	   |
	   |	output [31:0] dataOut
	   |);
	   |reg[31:0] rdata;
	   |reg wr_flag;
	   |always@(negedge clk) begin
	   |	wr_flag = 0;
	   |end
	   |import "DPI-C" function int unsigned pmem_read(input int unsigned raddr);
	   |import "DPI-C" function void pmem_write(
	   |	input int unsigned waddr, input int unsigned wdata, input byte wmask);	
	   |always @(posedge clk) begin
	   |	if(wrEn & valid) begin
	   |		if(wr_flag == 0) pmem_write(addr, dataIn, wmask);
	   |		wr_flag = 1;
	   |	end
	   |end
	   |assign dataOut = rdata;
	   |always @(*) begin
	   |	if(valid) begin
	   |		case(wmask)
	   |			8'b00000001: begin
	   |				rdata = pmem_read(addr) & 32'h000000FF;
	   |				if(sOrU == 1) 	rdata[31:8] = {24{rdata[7]}};
	   |				else 			rdata = rdata;
	   |			end	
	   |			8'b00000011: begin
	   |				rdata = pmem_read(addr) & 32'h0000FFFF;
	   |				if(sOrU == 1) 	rdata[31:16] = {16{rdata[15]}};
	   |				else 			rdata = rdata;
	   |			end	
	   |			8'b00001111:	rdata = pmem_read(addr) & 32'hFFFFFFFF;
	   |			default: 		rdata = 32'd0;
	   |		endcase
	   |	end
	   |	else begin
	   |		rdata = 32'd0;
	   |	end
	   |end
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
