package interface

import chisel3._
import chisel3.util._
import chisel3.util.Decoupled

/* Bus */
class IFU2IDU extends Bundle {
    val inst = Output(UInt(32.W))
	val pc 	 = Output(UInt(32.W))
}

class IDU2EXU extends Bundle {
	val pc 		= Output(UInt(32.W))
	val rs1Data = Output(UInt(32.W))
	val rs2Data = Output(UInt(32.W))
	val imm 	= Output(UInt(32.W))
	val inst 	= Output(UInt(32.W))

	val regWR 	= Output(UInt(1.W))
	val srcAALU = Output(UInt(2.W))
	val srcBALU = Output(UInt(2.W))
	val ctrALU 	= Output(UInt(4.W))
	val branch 	= Output(UInt(4.W))
	val toReg 	= Output(UInt(2.W))
	val memWR 	= Output(UInt(1.W))
	val memValid= Output(UInt(1.W))
	val memOP 	= Output(UInt(3.W))
	val rs1Index= Output(UInt(5.W))
	val ecall 	= Output(UInt(1.W))
	val mret 	= Output(UInt(1.W))
	val csrEn 	= Output(UInt(1.W))
	val csrWr 	= Output(UInt(1.W))
	val csrOP 	= Output(UInt(1.W))
	val csrALUOP= Output(UInt(2.W))
}

class EXU2LSU extends Bundle {
	val pc 			= Output(UInt(32.W))
	val memData		= Output(UInt(32.W))
	val aluData		= Output(UInt(32.W))
	val csrWData	= Output(UInt(32.W))
	val csrData 	= Output(UInt(32.W))
	val immData 	= Output(UInt(32.W))
	val rs1Data 	= Output(UInt(32.W))
	val inst 		= Output(UInt(32.W))

	val regWR 		= Output(UInt(1.W))
	val memWR 		= Output(UInt(1.W))
	val memValid	= Output(UInt(1.W))
	val memOP 		= Output(UInt(3.W))
	val toReg 		= Output(UInt(2.W))
	val ecall 		= Output(UInt(1.W))
	val csrEn 		= Output(UInt(1.W))
	val csrWr 		= Output(UInt(1.W))
}

class LSU2WBU extends Bundle {
	val pc 			= Output(UInt(32.W))
	val memData		= Output(UInt(32.W))
	val aluData		= Output(UInt(32.W))
	val csrWData	= Output(UInt(32.W))
	val csrData 	= Output(UInt(32.W))
	val rs1Data 	= Output(UInt(32.W))
	val inst 		= Output(UInt(32.W))

	val regWR 		= Output(UInt(1.W))
	val toReg 		= Output(UInt(2.W))

	val ecall 		= Output(UInt(1.W))
	val csrEn 		= Output(UInt(1.W))
	val csrWr 		= Output(UInt(1.W))
	val fencei		= Output(UInt(1.W))
}

/* Normal */
class C2IFU extends Bundle {
    val memData = Input(UInt(32.W))
}

class IFU2Icache extends Bundle {
	val addr 	= Output(UInt(32.W))
	val enable	= Output(Bool())
	val oEnable	= Input(Bool())
	val inst 	= Input(UInt(32.W))
}

class IDU2BaseReg extends Bundle {
	val rs1Data = Input(UInt(32.W))
	val rs2Data = Input(UInt(32.W))

	val rs1Index = Output(UInt(4.W))
	val rs2Index = Output(UInt(4.W))
}

class EXU2CSR extends Bundle {
	val csrData	= Input(UInt(32.W))
	val mret 	= Output(UInt(1.W))
	val ecall 	= Output(UInt(1.W))
	val csr 	= Output(UInt(12.W))
}

class WBU2CSR extends Bundle {
	val pc 		= Output(UInt(32.W))
	val csrWData= Output(UInt(32.W))
	val csr 	= Output(UInt(12.W))
	val ecall 	= Output(UInt(1.W))
	val csrEn 	= Output(UInt(1.W))
	val csrWr 	= Output(UInt(1.W))
}

class WBU2BaseReg extends Bundle {
	val rdIndex = Output(UInt(4.W))
	val data 	= Output(UInt(32.W))
	val regWR 	= Output(UInt(1.W))
	val pc 		= Output(UInt(32.W))
}

class WBU2IFU extends Bundle {
	val nextPC	= Output(UInt(32.W))
}

class IFUSRAM extends Bundle {
	val ren 	= Input(UInt(1.W))
	val addr 	= Input(UInt(32.W))
	val valid 	= Output(UInt(1.W))
	val data 	= Output(UInt(32.W))
}
class WBUSRAM extends Bundle {
	val clk 	= Output(UInt(1.W))
	val raddr	= Output(UInt(32.W))
	val ren		= Output(UInt(1.W))
	val rdata	= Input(UInt(32.W))
	val rValid 	= Input(UInt(1.W))
	val waddr	= Output(UInt(32.W))
	val wdata 	= Output(UInt(32.W))
	val wen 	= Output(UInt(1.W))
	val wmask 	= Output(UInt(4.W))
	val wValid 	= Input(UInt(1.W))
}

class AXILite extends Bundle {
	/* AR */
	val arAddr	= Output(UInt(32.W))
	val arValid	= Output(Bool())
	val arReady	= Input(Bool())

	/* R */
	val rData	= Input(UInt(32.W))
	val rrEsp	= Input(UInt(2.W))
	val rValid	= Input(Bool())
	val rReady 	= Output(Bool())

	/* AW */
	val awAddr	= Output(UInt(32.W))
	val awValid	= Output(Bool())
	val awReady	= Input(Bool())

	/* W */
	val wData 	= Output(UInt(32.W))
	val wStrb 	= Output(UInt(4.W))
	val wValid 	= Output(Bool())
	val wReady 	= Input(Bool())

	/* B */
	val bResp 	= Input(UInt(2.W))
	val bValid 	= Input(Bool())
	val bReady 	= Output(Bool())
}

class AXI extends Bundle {
	/* AW */
	val awready	= Input(Bool())
	val awvalid	= Output(Bool())
	val awaddr	= Output(UInt(32.W))
	val awid 	= Output(UInt(4.W))
	val awlen 	= Output(UInt(8.W))
	val awsize 	= Output(UInt(3.W))
	val awburst = Output(UInt(2.W))

	/* W */
	val wready 	= Input(Bool())
	val wvalid 	= Output(Bool())
	val wdata 	= Output(UInt(32.W))
	val wstrb 	= Output(UInt(4.W))
	val wlast 	= Output(Bool())

	/* B */
	val bready 	= Output(Bool())
	val bvalid 	= Input(Bool())
	val bresp 	= Input(UInt(2.W))
	val bid 	= Input(UInt(4.W))

	/* AR */
	val arready 	= Input(Bool())
	val arvalid 	= Output(Bool())
	val araddr 	= Output(UInt(32.W))
	val arid 	= Output(UInt(4.W))
	val arlen 	= Output(UInt(8.W))
	val arsize 	= Output(UInt(3.W))
	val arburst 	= Output(UInt(2.W))
	
	/* R */
	val rready 	= Output(Bool())
	val rvalid 	= Input(Bool())
	val rresp 	= Input(UInt(2.W))
	val rdata 	= Input(UInt(32.W))
	val rlast 	= Input(Bool())
	val rid 		= Input(UInt(4.W))
}

object AXIUtils {
    def initializeAXISlave(axi: AXI): Unit = {
        /* AW */
        axi.awready := false.B
        /* W */
        axi.wready := false.B
        /* B */
        axi.bvalid := false.B
        axi.bresp := 0.U
        axi.bid := 0.U
        /* AR */
        axi.arready := false.B
        /* R */
        axi.rvalid := false.B
        axi.rresp := 0.U
        axi.rdata := 0.U
        axi.rlast := false.B
        axi.rid := 0.U
    }

    def initializeAXIMaster(axi: AXI): Unit = {
        /* AW */
        axi.awvalid := false.B
        axi.awaddr := 0.U
        axi.awid := 0.U
        axi.awlen := 0.U
        axi.awsize := 2.U
        axi.awburst := 1.U
        /* W */
        axi.wvalid := false.B
        axi.wdata := 0.U
        axi.wstrb := 0.U
        axi.wlast := false.B
        /* B */
        axi.bready := false.B
        /* AR */
        axi.arvalid := false.B
        axi.araddr := 0.U
        axi.arid := 0.U
        axi.arlen := 0.U
        axi.arsize := 2.U
        axi.arburst := 1.U
        /* R */
        axi.rready := false.B
    }

	def initializeAXILite(axiLite: AXILite): Unit = {
		axiLite.arAddr := 0.U
		axiLite.arValid := false.B
		axiLite.rReady := false.B
		axiLite.awAddr := 0.U
		axiLite.awValid := false.B
		axiLite.wData := 0.U
		axiLite.wStrb := 0.U
		axiLite.wValid := false.B
		axiLite.bReady := false.B
	}
}
