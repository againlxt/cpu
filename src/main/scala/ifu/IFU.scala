package ifu

import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
import memory._
import javax.management.modelmbean.ModelMBean
import basemode.Delay
import basemode.AXIAccessFault
import cpu.Config
import dpic._
import dpic.PerformanceCounterType.{IFUGETINST => IFUGETINST}
class IFU extends Module {
    val io = IO(new Bundle {
		val pc  	 	= Input(UInt(32.W))
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Mem		= new AXI 
    })
	
	val pcReg = RegInit(0.U(32.W))
	pcReg := io.pc

	val icache = Module(new Icache(16, 128, 4, 4, 4, 16))
	icache.io.addr 		:= io.pc
	icache.io.enable	:= (pcReg =/= io.pc)
	icache.io.icache2Mem <> io.ifu2Mem

	if(Config.hasDPIC & (!Config.isSTA)) {
		val axiAccessFault = Module(new AXIAccessFault())
		axiAccessFault.io.ready := io.ifu2Mem.bready
		axiAccessFault.io.valid := io.ifu2Mem.bready
		axiAccessFault.io.resp	:= io.ifu2Mem.bresp
	}

	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val ifuGetInstCounter = RegInit(0.U(32.W))
		when (io.ifu2Mem.arvalid && io.ifu2Mem.arready) {
			ifuGetInstCounter := 0.U
		} .otherwise {
			ifuGetInstCounter := ifuGetInstCounter + 1.U
		}
		val IGIC 			= Module(new PerformanceCounter)
		IGIC.io.valid		:= io.ifu2Mem.rvalid && io.ifu2Mem.rready
		IGIC.io.counterType	:= PerformanceCounterType.IFUGETINST.asUInt
		IGIC.io.data 		:= ifuGetInstCounter
	}

	io.inst.valid		:= icache.io.oEnable
	io.inst.bits.inst	:= icache.io.inst
	io.inst.bits.pc		:= pcReg
}

/**
  * @param numOfCaches	:Number of caches
  * @param sizeOfCache	:Size of cache(bit)
  * @param m			:sizeOfCache/8 = 2^m
  * @param n			:numOfCaches = 2^n
  * @param burstLen		:AXI size
  * @param burstSize	:burst size(byte)
  */
class Icache(numOfCache: Int, sizeOfCache: Int, m: Int, n: Int, burstLen: Int, burstSize: Int) extends Module {
    val io = IO(new Bundle {
        val addr        = Input(UInt(32.W))
        val enable      = Input(Bool())
        val inst        = Output(UInt(32.W))
        val oEnable     = Output(Bool())
        val icache2Mem  = new AXI
    })
	val cacheValidReg   = RegInit(VecInit(Seq.fill(numOfCache)(false.B)))
	val tagReg 			= RegInit(VecInit(Seq.fill(numOfCache)(0.U((32-m-n).W))))
    val cache           = RegInit(VecInit(Seq.fill(numOfCache)(VecInit(Seq.fill(burstSize >> 2)(0.U(32.W))))))
    val addrReg         = RegInit(0.U(32.W))
	val indexWire 		= addrReg(m+n-1, m)
	val offset 			= addrReg(m-1,0) >> 2

    val s_idle   = "b00001".U
    val s_check  = "b00010".U
    val s_find   = "b00100".U
	val s_find_b = "b01000".U
    val s_output = "b10000".U
    val state       = RegInit(1.U(5.W))
    val hitWire     = (addrReg(31,m+n) === tagReg(indexWire)) && cacheValidReg(indexWire)
    val findEndWire = Wire(Bool())
	val busrtCnt 	  	= RegInit(0.U(8.W))
	if(Config.SoC) {
		findEndWire := io.icache2Mem.rvalid & io.icache2Mem.rready & io.icache2Mem.rlast & (busrtCnt === ((burstSize.U >> 2)-1.U))
	} else {
		findEndWire := io.icache2Mem.rvalid & io.icache2Mem.rready
	}
	val isSdram 	= (addrReg(31,28) >= 10.U)
    state := MuxLookup(state, s_idle)(List(
        s_idle      -> Mux(io.enable, s_check, s_idle),
        s_check     -> Mux(hitWire, s_output, Mux(isSdram, s_find_b, s_find)),
        s_find      -> Mux(findEndWire, s_output, s_find),
		s_find_b    -> Mux(findEndWire, s_output, s_find_b),
        s_output    -> s_idle
    ))

    /* AW */
	val awvalidReg		= RegInit(0.B)
	val awaddrReg		= RegInit(0.U(32.W))
	val awidReg 		= RegInit(0.U(4.W))
	val awlenReg 		= RegInit(0.U(8.W))
	val awsizeReg 		= RegInit(2.U(3.W))
	val awburstReg 		= RegInit(1.U(2.W))
	/* W */
	val wvalidReg		= RegInit(0.B)
	val wdataReg		= RegInit(0.U(32.W))
	val wstrbReg		= RegInit(15.U(4.W))
	val wlastReg 		= RegInit(0.B)
	/* B */
	val breadyReg		= RegInit(1.B)
	/* AR */
	val arvalidReg		= RegInit(0.U(1.W))
	val aridReg 		= RegInit(0.U(4.W))
	val arlenReg 		= RegInit(0.U(8.W))
	val arsizeReg 		= RegInit(2.U(3.W))
	val arburstReg 		= RegInit(1.U(2.W))
	/* R */
	val rreadyReg		= RegInit(0.B)

    /* Signal Connection */
	/* AW */
	val awreadyWire		    = io.icache2Mem.awready
	io.icache2Mem.awvalid	:= awvalidReg
	io.icache2Mem.awaddr	:= awaddrReg
	io.icache2Mem.awid 	    := awidReg
	io.icache2Mem.awlen 	:= awlenReg
	io.icache2Mem.awsize 	:= awsizeReg
	io.icache2Mem.awburst	:= awburstReg
	/* W */
	val wreadyWire 		= io.icache2Mem.wready
	io.icache2Mem.wvalid 	:= wvalidReg
	io.icache2Mem.wdata 	:= wdataReg
	io.icache2Mem.wstrb 	:= wstrbReg
	io.icache2Mem.wlast 	:= wlastReg
	/* B */
	io.icache2Mem.bready	:= breadyReg
	val bvalidWire 		= io.icache2Mem.bvalid
	val brespWire 		= io.icache2Mem.bresp
	val bidWire 		= io.icache2Mem.bid
	/* AR */
	val arreadyWire 	    = io.icache2Mem.arready
	io.icache2Mem.arvalid	:= arvalidReg
	io.icache2Mem.araddr	:= Mux(isSdram, Cat(addrReg(31,4), 0.U(4.W)), Cat(addrReg(31,4), 0.U(4.W)) + (busrtCnt << 2)) 
	io.icache2Mem.arid 	    := aridReg
	io.icache2Mem.arlen 	:= arlenReg
	io.icache2Mem.arsize 	:= arsizeReg
	io.icache2Mem.arburst	:= arburstReg
	/* R */
	io.icache2Mem.rready 	:= rreadyReg
	val rvalidWire 		= io.icache2Mem.rvalid
	val rrespWire 		= io.icache2Mem.rresp
	val rdataWire 		= io.icache2Mem.rdata
	val rlastWire 		= io.icache2Mem.rlast
	val ridWire 		= io.icache2Mem.rid

    /* State Machine */
    when(io.enable) {
		addrReg := io.addr
	}
	when(io.icache2Mem.rvalid & io.icache2Mem.rready) { 
		cache(addrReg(m+n-1, m))(busrtCnt) := rdataWire
		when(busrtCnt === ((burstSize.U >> 2)-1.U)) {
			busrtCnt := 0.U
		} .otherwise {
			busrtCnt := busrtCnt + 1.U
		}
	}
    switch(state) {
        is(s_check) { cacheValidReg(indexWire)                     := 
        Mux(addrReg(31,m+n) === tagReg(indexWire), cacheValidReg(indexWire), false.B) }
        is(s_find) { cacheValidReg(addrReg(m+n-1, m))  := findEndWire}
		is(s_find_b) { cacheValidReg(addrReg(m+n-1, m))  := findEndWire }
    }
	switch(state) {
		is(s_find) { tagReg(indexWire) := addrReg(31,m+n) }
		is(s_find_b) { tagReg(indexWire) := addrReg(31,m+n) }
	}
    switch(state) {
        is(s_check) {
			arvalidReg 	:= !hitWire
			arlenReg 	:= Mux(isSdram, burstLen.U-1.U, 0.U)
		}
        is(s_find)  {
			when (io.icache2Mem.arvalid & io.icache2Mem.arready) {
				arvalidReg := 0.B
			} .elsewhen(io.icache2Mem.rvalid & io.icache2Mem.rready & (busrtCnt < ((burstSize.U >> 2)-1.U))) {
				arvalidReg := 1.B
			}
		}
		is(s_find_b) {
			when (io.icache2Mem.arvalid & io.icache2Mem.arready) {
				arvalidReg := 0.B
			}
		}
    }
    switch(rreadyReg) {
        is(0.B) { rreadyReg := rvalidWire }
        is(1.B) { rreadyReg := !(rvalidWire & rlastWire) }
    }
	switch(breadyReg) {
		is(0.B) { breadyReg := bvalidWire }
		is(1.B) { breadyReg := !(bvalidWire) }
	}
	if(Config.hasDPIC & (!Config.isSTA)) {
		val axiAccessFault = Module(new AXIAccessFault())
		axiAccessFault.io.ready := breadyReg
		axiAccessFault.io.valid := bvalidWire
		axiAccessFault.io.resp	:= brespWire
	}
	/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val accessTimeCounter 	= RegInit(0.U(32.W))
		val missPenaltyCounter	= RegInit(0.U(32.W))
		val hitRateCounter 		= RegInit(0.U(32.W))

		val hitReg 				= RegInit(0.B)

		switch(state) {
			is(s_check) { 
				hitReg := hitWire 
				accessTimeCounter := 2.U
			}
		}
		switch(state) {
			is(s_find) { 
				missPenaltyCounter := missPenaltyCounter + 1.U
			}
			is(s_find_b) {
				missPenaltyCounter := missPenaltyCounter + 1.U
			}
			is(s_output) { missPenaltyCounter := 0.U }
		}
		

		val ATC 			= Module(new PerformanceCounter)
		ATC.io.valid		:= hitReg & (state === s_output)
		ATC.io.counterType	:= PerformanceCounterType.ICACHE_ACCESS_TIME.asUInt
		ATC.io.data 		:= accessTimeCounter
		val MPC 			= Module(new PerformanceCounter)
		MPC.io.valid		:= !hitReg & (state === s_output)
		MPC.io.counterType	:= PerformanceCounterType.ICACHE_MISS_PENALTY.asUInt
		MPC.io.data 		:= missPenaltyCounter
	}

    io.oEnable := (state === s_output)
    io.inst    := cache(addrReg(m+n-1, m))(offset)
}
