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

class EXU2WBU extends Bundle {
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
	val branchCtr 	= Output(UInt(4.W))
	val less 		= Output(UInt(1.W))
	val zero 		= Output(UInt(1.W))
	val ecall 		= Output(UInt(1.W))
	val csrEn 		= Output(UInt(1.W))
	val csrWr 		= Output(UInt(1.W))
}

/* Normal */
class C2IFU extends Bundle {
    val memData = Input(UInt(32.W))
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
}

class WBU2PC extends Bundle {
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

class AXIMaster extends Bundle {
	/* AW */
	val master_awready	= Input(Bool())
	val master_awvalid	= Output(Bool())
	val master_awaddr	= Output(UInt(32.W))
	val master_awid 	= Output(UInt(4.W))
	val master_awlen 	= Output(UInt(8.W))
	val master_awsize 	= Output(UInt(3.W))
	val master_awburst 	= Output(UInt(2.W))

	/* W */
	val master_wready 	= Input(Bool())
	val master_wvalid 	= Output(Bool())
	val master_wdata 	= Output(UInt(32.W))
	val master_wstrb 	= Output(UInt(4.W))
	val master_wlast 	= Output(Bool())

	/* B */
	val master_bready 	= Output(Bool())
	val master_bvalid 	= Input(Bool())
	val master_bresp 	= Input(UInt(2.W))
	val master_bid 		= Input(UInt(4.W))

	/* AR */
	val master_arready 	= Input(Bool())
	val master_arvalid 	= Output(Bool())
	val master_araddr 	= Output(UInt(32.W))
	val master_arid 	= Output(UInt(4.W))
	val master_arlen 	= Output(UInt(8.W))
	val master_arsize 	= Output(UInt(3.W))
	val master_arburst 	= Output(UInt(2.W))
	
	/* R */
	val master_rready 	= Output(Bool())
	val master_rvalid 	= Input(Bool())
	val master_rresp 	= Input(UInt(2.W))
	val master_rdata 	= Input(UInt(32.W))
	val master_rlast 	= Input(Bool())
	val master_rid 		= Input(UInt(4.W))
}

class AXISlave extends Bundle {
	/* AW */
	val slave_awready	= Output(Bool())
	val slave_awvalid	= Input(Bool())
	val slave_awaddr	= Input(UInt(32.W))
	val slave_awid 		= Input(UInt(4.W))
	val slave_awlen 	= Input(UInt(8.W))
	val slave_awsize 	= Input(UInt(3.W))
	val slave_awburst 	= Input(UInt(2.W))

	/* W */
	val slave_wready 	= Output(Bool())
	val slave_wvalid 	= Input(Bool())
	val slave_wdata 	= Input(UInt(32.W))
	val slave_wstrb 	= Input(UInt(4.W))
	val slave_wlast 	= Input(Bool())

	/* B */
	val slave_bready 	= Input(Bool())
	val slave_bvalid 	= Output(Bool())
	val slave_bresp 	= Output(UInt(2.W))
	val slave_bid 		= Output(UInt(4.W))

	/* AR */
	val slave_arready 	= Output(Bool())
	val slave_arvalid 	= Input(Bool())
	val slave_araddr 	= Input(UInt(32.W))
	val slave_arid 		= Input(UInt(4.W))
	val slave_arlen 	= Input(UInt(8.W))
	val slave_arsize 	= Input(UInt(3.W))
	val slave_arburst 	= Input(UInt(2.W))
	
	/* R */
	val slave_rready 	= Input(Bool())
	val slave_rvalid 	= Output(Bool())
	val slave_rresp 	= Output(UInt(2.W))
	val slave_rdata 	= Output(UInt(32.W))
	val slave_rlast 	= Output(Bool())
	val slave_rid 		= Output(UInt(4.W))
}

object AXIUtils {
    def initializeAXISlave(axiSlave: AXISlave): Unit = {
        /* AW */
        axiSlave.slave_awready := false.B
        /* W */
        axiSlave.slave_wready := false.B
        /* B */
        axiSlave.slave_bvalid := false.B
        axiSlave.slave_bresp := 0.U
        axiSlave.slave_bid := 0.U
        /* AR */
        axiSlave.slave_arready := false.B
        /* R */
        axiSlave.slave_rvalid := false.B
        axiSlave.slave_rresp := 0.U
        axiSlave.slave_rdata := 0.U
        axiSlave.slave_rlast := false.B
        axiSlave.slave_rid := 0.U
    }

    def initializeAXIMaster(axiMaster: AXIMaster): Unit = {
        /* AW */
        axiMaster.master_awvalid := false.B
        axiMaster.master_awaddr := 0.U
        axiMaster.master_awid := 0.U
        axiMaster.master_awlen := 0.U
        axiMaster.master_awsize := 2.U
        axiMaster.master_awburst := 1.U
        /* W */
        axiMaster.master_wvalid := false.B
        axiMaster.master_wdata := 0.U
        axiMaster.master_wstrb := 0.U
        axiMaster.master_wlast := false.B
        /* B */
        axiMaster.master_bready := false.B
        /* AR */
        axiMaster.master_arvalid := false.B
        axiMaster.master_araddr := 0.U
        axiMaster.master_arid := 0.U
        axiMaster.master_arlen := 0.U
        axiMaster.master_arsize := 2.U
        axiMaster.master_arburst := 1.U
        /* R */
        axiMaster.master_rready := false.B
    }

	def connectAXI(master: AXIMaster, slave: AXISlave): Unit = {
		/* AW channel */
		master.master_awready <> slave.slave_awready
		slave.slave_awvalid <> master.master_awvalid
		slave.slave_awaddr <> master.master_awaddr
		slave.slave_awid <> master.master_awid
		slave.slave_awlen <> master.master_awlen
		slave.slave_awsize <> master.master_awsize
		slave.slave_awburst <> master.master_awburst

		/* W channel */
		master.master_wready <> slave.slave_wready
		slave.slave_wvalid <> master.master_wvalid
		slave.slave_wdata <> master.master_wdata
		slave.slave_wstrb <> master.master_wstrb
		slave.slave_wlast <> master.master_wlast

		/* B channel */
		slave.slave_bready <> master.master_bready
		master.master_bvalid <> slave.slave_bvalid
		master.master_bresp <> slave.slave_bresp
		master.master_bid <> slave.slave_bid

		/* AR channel */
		master.master_arready <> slave.slave_arready
		slave.slave_arvalid <> master.master_arvalid
		slave.slave_araddr <> master.master_araddr
		slave.slave_arid <> master.master_arid
		slave.slave_arlen <> master.master_arlen
		slave.slave_arsize <> master.master_arsize
		slave.slave_arburst <> master.master_arburst

		/* R channel */
		slave.slave_rready <> master.master_rready
		master.master_rvalid <> slave.slave_rvalid
		master.master_rresp <> slave.slave_rresp
		master.master_rdata <> slave.slave_rdata
		master.master_rlast <> slave.slave_rlast
		master.master_rid <> slave.slave_rid
	}

}
