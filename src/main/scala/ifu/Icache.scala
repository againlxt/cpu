package ifu
import chisel3._
import chisel3.util._
import interface._
import _root_.interface._
import memory._
import cpu.Config
import basemode.AXIAccessFault
import dpic.PerformanceCounter
import dpic.PerformanceCounterType

/**
  * @param numOfCaches	:Number of caches
  * @param sizeOfCache	:Size of cache(bit)
  * @param m			:sizeOfCache/8 = 2^m
  * @param n			:numOfCaches = 2^n
  * @param burstLen		:AXI size
  * @param burstSize	:burst size(byte)
  */
class Icache(numOfCache: Int, sizeOfCache: Int, m: Int, n: Int, burstLen: Int, burstSize: Int, way: Int, policy: ReplacePolicy.Type) extends Module {
    val io = IO(new Bundle {
        val ifu2ICache	= Flipped(Decoupled(new IFU2ICache))
		val icache2IFU 	= Decoupled(new ICache2IFU)
        val icache2Mem  = new AXI
		val wbu2Icache	= Input(Bool())
		val flush 		= Input(Bool())
    })
	val cacheValidReg 	= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(false.B)))))
	val tagReg   		= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(0.U((32-m-n).W))))))
	val cache  			= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(VecInit(Seq.fill(burstSize >> 2)(0.U(32.W))))))))

    val addrReg         = RegEnable(io.ifu2ICache.bits.pc, io.ifu2ICache.valid & io.ifu2ICache.ready)
	val tagWire			= addrReg(31, m+n)
	val indexWire 		= addrReg(m+n-1, m)
	val offsetWire 		= addrReg(m-1,0) >> 2
	val wayValidVec 	= cacheValidReg(indexWire)
	val wayTagVec		= tagReg(indexWire)
	val hitVec 			= wayValidVec.zip(wayTagVec).map { case (v, t) => v && (t === tagWire) }
	val hitWire     	= hitVec.reduce(_ || _)
	val hitWay 			= PriorityEncoder(hitVec)
	val flushReg 		= RegInit(0.B)

	val s_idle   = "b00001".U
    val s_check  = "b00010".U
    val s_find   = "b00100".U
	val s_find_b = "b01000".U
	val s_wait 	 = "b10000".U
    val state       = RegInit(1.U(5.W))

	val sets 			= numOfCache/way
	val wayIndexWidth 	= log2Up(way)
	val replacement_algorithm	= Module(new Replacement_Algorithm
	(way, sets, n, policy))
	replacement_algorithm.io.update_entry 	:= !hitWire & (state === s_check)
	replacement_algorithm.io.update_index	:= indexWire
	val wayIndex 		= replacement_algorithm.io.way_index

    val findEndWire = Wire(Bool())
	val busrtCnt 	  	= RegInit(0.U(8.W))
	if(Config.SoC) {
		findEndWire := io.icache2Mem.rvalid & io.icache2Mem.rready & io.icache2Mem.rlast & (busrtCnt === ((burstSize.U >> 2)-1.U))
	} else {
		findEndWire := io.icache2Mem.rvalid & io.icache2Mem.rready
	}
	val isSdram 	= (addrReg(31,28) >= 10.U)
    state := MuxLookup(state, s_idle)(List(
        s_idle      -> Mux(io.ifu2ICache.valid & io.ifu2ICache.ready, s_check, s_idle),
        s_check     -> Mux(hitWire, s_wait, Mux(isSdram, s_find_b, s_find)),
        s_find      -> Mux(findEndWire, s_wait, s_find),
		s_find_b    -> Mux(findEndWire, s_wait, s_find_b),
		s_wait 		-> Mux((io.icache2IFU.valid & io.icache2IFU.ready) | io.flush | flushReg, s_idle, s_wait)
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
	io.icache2Mem.bready	:= 0.B
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
	when(io.icache2Mem.rvalid & io.icache2Mem.rready) { 
		cache(addrReg(m+n-1, m))(wayIndex)(busrtCnt) := rdataWire
		when(busrtCnt === ((burstSize.U >> 2)-1.U)) {
			busrtCnt := 0.U
		} .otherwise {
			busrtCnt := busrtCnt + 1.U
		}
	}
	when(io.wbu2Icache) {
		for (i <- 0 until (numOfCache / way)) {
			for (j <- 0 until way) {
			    cacheValidReg(i)(j) := false.B
			}
		}
	} .otherwise {
		switch(state) {
			is(s_check) { cacheValidReg(indexWire)(wayIndex)	:= 
			Mux(addrReg(31,m+n) === tagReg(indexWire)(wayIndex), cacheValidReg(indexWire)(wayIndex), false.B)}
			is(s_find) { cacheValidReg(addrReg(m+n-1, m))(wayIndex)  	:= findEndWire}
			is(s_find_b) { cacheValidReg(addrReg(m+n-1, m))(wayIndex)  	:= findEndWire }
		}
	}
	switch(state) {
		is(s_find) { tagReg(indexWire)(wayIndex) := addrReg(31,m+n) }
		is(s_find_b) { tagReg(indexWire)(wayIndex) := addrReg(31,m+n) }
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
                missPenaltyCounter := Mux(findEndWire, 0.U, missPenaltyCounter + 1.U) 
			}
			is(s_find_b) {
				missPenaltyCounter := Mux(findEndWire, 0.U, missPenaltyCounter + 1.U) 
			}
		}
		

		val ATC 			= Module(new PerformanceCounter)
		ATC.io.valid		:= hitWire & (state === s_check)
		ATC.io.counterType	:= PerformanceCounterType.ICACHE_ACCESS_TIME.asUInt
		ATC.io.data 		:= accessTimeCounter
		val MPC 			= Module(new PerformanceCounter)
		MPC.io.valid		:= !hitReg & (findEndWire)
		MPC.io.counterType	:= PerformanceCounterType.ICACHE_MISS_PENALTY.asUInt
		MPC.io.data 		:= missPenaltyCounter
	}
	val oValidReg	= RegInit(0.B)
	val rdataReg    = RegEnable(io.icache2Mem.rdata, findEndWire)
	when((state === s_wait) | (state === s_idle)) {
		flushReg := 0.B
	} .otherwise {
		flushReg := Mux(flushReg, flushReg, io.flush)
	}
	switch(state) {
		is(s_idle) {
			oValidReg 	:= 0.B
		}
		is(s_check){
			oValidReg 	:= hitWire
		}
		is(s_find) {
			oValidReg := findEndWire
		}
		is(s_find_b) {
			oValidReg := findEndWire
		}
		is(s_wait) {
			oValidReg := Mux((io.icache2IFU.valid & io.icache2IFU.ready) | io.flush | flushReg, 0.B, oValidReg)
		}
	}

	io.ifu2ICache.ready			:= (state === s_idle)
    io.icache2IFU.valid 		:= oValidReg & !(io.flush | flushReg)
    io.icache2IFU.bits.inst    	:= Mux(((state =/= s_check) & findEndWire & (offsetWire === 3.U))
    , rdataReg, cache(addrReg(m+n-1, m))(wayIndex)(offsetWire))
	io.icache2IFU.bits.pc		:= addrReg
}

class IcachePipe(numOfCache: Int, sizeOfCache: Int, m: Int, n: Int, burstLen: Int, burstSize: Int, way: Int, policy: ReplacePolicy.Type) extends Module {
    val io = IO(new Bundle {
        val ifu2ICache	= Flipped(Decoupled(new IFU2ICache))
		val icache2IFU 	= Decoupled(new ICache2IFU)
        val icache2Mem  = new AXI
		val wbu2Icache	= Input(Bool())
		val flush 		= Input(Bool())
    })
	def pipelineConnect[T <: Data, T2 <: Data](prevOut: DecoupledIO[T],
	thisIn: DecoupledIO[T]) = {
		prevOut.ready 	:= thisIn.ready
		thisIn.bits 	:= RegEnable(prevOut.bits, prevOut.valid & thisIn.ready)
		thisIn.valid 	:= prevOut.valid & thisIn.ready
	}

	val cacheMem 	= Seq.fill(way)(SyncReadMem(numOfCache/way, Vec(burstSize >> 2, UInt(32.W))))
	val tagMem 		= Seq.fill(way)(SyncReadMem(numOfCache/way, UInt((32-m-n).W)))
	val fetchReq 	= Module(new FetchReq)
	val checkUnit	= Module(new CheckUnit(numOfCache, sizeOfCache, m, n, burstLen, burstSize, way, policy))
	val preDecoder 	= Module(new PreDecoder)

	val indexWire 			= fetchReq.io.fetchReqIO.fetchReq2CheckUnit.bits(m+n-1, m)
	val fetchReqHandWire	= fetchReq.io.fetchReqIO.fetchReq2CheckUnit.valid & 
	fetchReq.io.fetchReqIO.fetchReq2CheckUnit.ready
	val cacheLineVec = cacheMem.map(mem => mem.read(indexWire, fetchReqHandWire))
	val tagVec       = tagMem.map(mem => mem.read(indexWire, fetchReqHandWire))	

	checkUnit.io.checkUnitIO.checkUnit2Mem <> io.icache2Mem
	checkUnit.io.checkUnitIO.checkUnit2Sram.cacheLineVec 	:= cacheLineVec
	checkUnit.io.checkUnitIO.checkUnit2Sram.tagVec			:= tagVec
	val replaceWay 		= checkUnit.io.checkUnitIO.checkUnit2Sram.replaceWay
	val replaceIndex	= 
	for (w <- 0 until way) {
		when (replaceWay === w.U & checkUnit.io.checkUnitIO.checkUnit2Sram.wen) {
			cacheMem(w).write(checkUnit.io.checkUnitIO.checkUnit2Sram.replaceIndex, checkUnit.io.checkUnitIO.checkUnit2Sram.cacheBuf)
			tagMem(w).write(checkUnit.io.checkUnitIO.checkUnit2Sram.replaceIndex, checkUnit.io.checkUnitIO.checkUnit2Sram.tagBuf)
		}
	}
	fetchReq.io.fetchReqIO.ifu2FetchReq <> io.ifu2ICache
	fetchReq.io.fetchReqIO.flush		:= io.flush
	checkUnit.io.checkUnitIO.flush		:= io.flush
	checkUnit.io.checkUnitIO.wbu2Icache	:= io.wbu2Icache
	preDecoder.io.preDecoderIO.flush	:= io.flush
	pipelineConnect(fetchReq.io.fetchReqIO.fetchReq2CheckUnit, checkUnit.io.checkUnitIO.fetchReq2CheckUnit)
	pipelineConnect(checkUnit.io.checkUnitIO.checkUnit2PreDecoder, preDecoder.io.preDecoderIO.checkUnit2PreDecoder)
	preDecoder.io.preDecoderIO.preDecoder2IFU <> io.icache2IFU
}

class FetchReq extends Module {
	val io = IO(new Bundle {
		val fetchReqIO = new FetchReqIO
	})
	val validReg = RegInit(0.B)
	val readyReg = RegInit(1.B)
	val pcReg 	 = RegEnable(io.fetchReqIO.ifu2FetchReq.bits.pc, 
	io.fetchReqIO.ifu2FetchReq.valid & io.fetchReqIO.ifu2FetchReq.ready)
	when(io.fetchReqIO.flush) {
		validReg := 0.B
		readyReg := 1.B
	} .otherwise {
		switch(validReg) {
			is(0.B) { validReg := io.fetchReqIO.ifu2FetchReq.valid & io.fetchReqIO.ifu2FetchReq.ready}
			is(1.B) {
				validReg := Mux(io.fetchReqIO.fetchReq2CheckUnit.valid & io.fetchReqIO.fetchReq2CheckUnit.ready,
				Mux(io.fetchReqIO.ifu2FetchReq.valid & io.fetchReqIO.ifu2FetchReq.ready, 1.B, 0.B), 1.B)
			}
		}
		switch(readyReg) {
			is(0.B) { readyReg := io.fetchReqIO.fetchReq2CheckUnit.valid & io.fetchReqIO.fetchReq2CheckUnit.ready }
			is(1.B) {
				readyReg := Mux(io.fetchReqIO.ifu2FetchReq.valid & io.fetchReqIO.ifu2FetchReq.ready, 
				Mux(io.fetchReqIO.fetchReq2CheckUnit.valid & io.fetchReqIO.fetchReq2CheckUnit.ready, 1.B, 0.B), 1.B)
			}
		}
	}

	io.fetchReqIO.ifu2FetchReq.ready 		:= readyReg
	io.fetchReqIO.fetchReq2CheckUnit.bits	:= pcReg
	io.fetchReqIO.fetchReq2CheckUnit.valid	:= validReg & (!io.fetchReqIO.flush)
}

class CheckUnit(numOfCache: Int, sizeOfCache: Int, m: Int, n: Int, burstLen: Int, burstSize: Int, way: Int, policy: ReplacePolicy.Type) extends Module {
	val io = IO(new Bundle {
		val checkUnitIO = new CheckUnitIO
	})
	val feq2CheckHandWire	= io.checkUnitIO.fetchReq2CheckUnit.valid & io.checkUnitIO.fetchReq2CheckUnit.ready
	val feq2CheckHandReg 	= RegNext(feq2CheckHandWire)
	val pcWire 				= io.checkUnitIO.fetchReq2CheckUnit.bits
	val memIndexWire 		= io.checkUnitIO.fetchReq2CheckUnit.bits(m+n-1, m)
	val tagWire 			= pcWire(31, m+n)
	val indexWire 			= pcWire(m+n-1, m)
	val offsetWire 			= pcWire(m-1,0) >> 2

	val cacheValidReg	= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(false.B)))))
	val cacheLineRegVec = io.checkUnitIO.checkUnit2Sram.cacheLineVec
	val wayTagRegVec 	= io.checkUnitIO.checkUnit2Sram.tagVec
	val wayValidVec 	= cacheValidReg(memIndexWire)
	val hitVec 			= wayValidVec.zip(wayTagRegVec).map { case (v, t) => v && (t === tagWire) }
	val hitWire 		= hitVec.reduce(_ || _)
	val hitWay 			= PriorityEncoder(hitVec)

	val axiHitVecReg 	= RegInit(VecInit(Seq.fill(burstLen)(false.B)))
	val axiDataVecReg	= RegInit(VecInit(Seq.fill(burstLen)(0.U(32.W))))
	val busrtCnt 	  	= RegInit(0.U(8.W))
	val findEndWire		= io.checkUnitIO.checkUnit2Mem.rvalid & io.checkUnitIO.checkUnit2Mem.rready &
	io.checkUnitIO.checkUnit2Mem.rlast & (busrtCnt === ((burstSize.U >> 2)-1.U))

	val s_flow 	= "b001".U
	val s_miss 	= "b010".U
	val s_load 	= "b100".U
	val nextState = WireInit(1.U(3.W))
	val state 	= RegNext(nextState)
	nextState 	:= MuxLookup(state, s_flow)(List(
		s_flow 	-> Mux((!hitWire) & feq2CheckHandReg, s_miss, s_flow),
		s_miss	-> Mux(findEndWire, s_load, s_miss),
		s_load	-> s_flow
	))

	/* Replace */
	val replaceWay = WireInit(0.U)
	/* ReplaceEnd */

	/* AXI */
	val isSdram 		= (pcWire(31,28) >= 10.U)
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
	val awreadyWire		    = io.checkUnitIO.checkUnit2Mem.awready
	io.checkUnitIO.checkUnit2Mem.awvalid	:= awvalidReg
	io.checkUnitIO.checkUnit2Mem.awaddr		:= awaddrReg
	io.checkUnitIO.checkUnit2Mem.awid 	    := awidReg
	io.checkUnitIO.checkUnit2Mem.awlen 		:= awlenReg
	io.checkUnitIO.checkUnit2Mem.awsize 	:= awsizeReg
	io.checkUnitIO.checkUnit2Mem.awburst	:= awburstReg
	/* W */
	val wreadyWire 		= io.checkUnitIO.checkUnit2Mem.wready
	io.checkUnitIO.checkUnit2Mem.wvalid := wvalidReg
	io.checkUnitIO.checkUnit2Mem.wdata 	:= wdataReg
	io.checkUnitIO.checkUnit2Mem.wstrb 	:= wstrbReg
	io.checkUnitIO.checkUnit2Mem.wlast 	:= wlastReg
	/* B */
	io.checkUnitIO.checkUnit2Mem.bready	:= breadyReg
	val bvalidWire 		= io.checkUnitIO.checkUnit2Mem.bvalid
	val brespWire 		= io.checkUnitIO.checkUnit2Mem.bresp
	val bidWire 		= io.checkUnitIO.checkUnit2Mem.bid
	/* AR */
	val arreadyWire 	= io.checkUnitIO.checkUnit2Mem.arready
	io.checkUnitIO.checkUnit2Mem.arvalid	:= arvalidReg
	io.checkUnitIO.checkUnit2Mem.araddr		:= 
	Mux(isSdram, Cat(pcWire(31,4), 0.U(4.W)), Cat(pcWire(31,4), 0.U(4.W)) + (busrtCnt << 2)) 
	io.checkUnitIO.checkUnit2Mem.arid 	    := aridReg
	io.checkUnitIO.checkUnit2Mem.arlen 		:= arlenReg
	io.checkUnitIO.checkUnit2Mem.arsize 	:= arsizeReg
	io.checkUnitIO.checkUnit2Mem.arburst	:= arburstReg
	/* R */
	io.checkUnitIO.checkUnit2Mem.rready 	:= rreadyReg
	val rvalidWire 		= io.checkUnitIO.checkUnit2Mem.rvalid
	val rrespWire 		= io.checkUnitIO.checkUnit2Mem.rresp
	val rdataWire 		= io.checkUnitIO.checkUnit2Mem.rdata
	val rlastWire 		= io.checkUnitIO.checkUnit2Mem.rlast
	val ridWire 		= io.checkUnitIO.checkUnit2Mem.rid

	/* AXI State Machine */
	switch(state) {
		is(s_miss) {
			when(io.checkUnitIO.checkUnit2Mem.rvalid & io.checkUnitIO.checkUnit2Mem.rready) { 
				axiDataVecReg(busrtCnt) := rdataWire
				axiHitVecReg(busrtCnt)	:= 1.B
				when(busrtCnt === ((burstSize.U >> 2)-1.U)) {
					busrtCnt := 0.U
				} .otherwise {
					busrtCnt := busrtCnt + 1.U
				}
			}
		}
		is(s_load) {
			axiDataVecReg 	:= VecInit(Seq.fill(burstLen)(0.U(32.W)))
			axiHitVecReg	:= VecInit(Seq.fill(burstLen)(false.B))
		}
	}
	when(io.checkUnitIO.wbu2Icache) {
		for (i <- 0 until (numOfCache / way)) {
			for (j <- 0 until way) {
			    cacheValidReg(i)(j) := false.B
			}
		}
	} .otherwise {
		when(state === s_load) {cacheValidReg(indexWire)(replaceWay) := 1.B}
	}
	switch(state) {
        is(s_flow) {
			arvalidReg 	:= (!hitWire) & feq2CheckHandReg
			arlenReg 	:= Mux((!hitWire) & feq2CheckHandReg, Mux(isSdram, burstLen.U-1.U, 0.U), arlenReg)
		}
        is(s_miss)  {
			when(isSdram) {
				when (io.checkUnitIO.checkUnit2Mem.arvalid & io.checkUnitIO.checkUnit2Mem.arready) {
					arvalidReg := 0.B
				}
			} .otherwise {
				when (io.checkUnitIO.checkUnit2Mem.arvalid & io.checkUnitIO.checkUnit2Mem.arready) {
					arvalidReg := 0.B
				} .elsewhen(io.checkUnitIO.checkUnit2Mem.rvalid & io.checkUnitIO.checkUnit2Mem.rready & (busrtCnt < ((burstSize.U >> 2)-1.U))) {
					arvalidReg := 1.B
				}
			}
		}
    }
	switch(rreadyReg) {
        is(0.B) { rreadyReg := rvalidWire }
        is(1.B) { rreadyReg := !(rvalidWire & rlastWire) }
    }

	/* AXI End */
	val validReg = RegInit(0.B)
	val readyReg = RegInit(1.B)
	when (io.checkUnitIO.flush) {
		validReg := 0.B
		readyReg := 1.B
	} .otherwise {
		switch(validReg) {
			is(0.B) { validReg := io.checkUnitIO.fetchReq2CheckUnit.valid & io.checkUnitIO.fetchReq2CheckUnit.ready}
			is(1.B) {
				validReg := Mux(io.checkUnitIO.checkUnit2PreDecoder.valid & io.checkUnitIO.checkUnit2PreDecoder.ready,
				Mux(io.checkUnitIO.fetchReq2CheckUnit.valid & io.checkUnitIO.fetchReq2CheckUnit.ready, 1.B, 0.B), 1.B)
			}
		}
		switch(readyReg) {
			is(0.B) { readyReg := io.checkUnitIO.checkUnit2PreDecoder.valid & io.checkUnitIO.checkUnit2PreDecoder.ready }
			is(1.B) {
				readyReg := Mux(io.checkUnitIO.fetchReq2CheckUnit.valid & io.checkUnitIO.fetchReq2CheckUnit.ready, 
				Mux(io.checkUnitIO.checkUnit2PreDecoder.valid & io.checkUnitIO.checkUnit2PreDecoder.ready, 1.B, 0.B), 1.B)
			}
		}
	}

	io.checkUnitIO.checkUnit2PreDecoder.valid 		:= validReg & 
	((hitWire & (state === s_flow)) | ((state =/= s_flow) & axiHitVecReg(offsetWire))) & 
	(!io.checkUnitIO.flush)
	io.checkUnitIO.checkUnit2PreDecoder.bits.inst	:= 
	Mux((state === s_flow), cacheLineRegVec(hitWay)(offsetWire), axiDataVecReg(offsetWire))
	io.checkUnitIO.checkUnit2PreDecoder.bits.pc		:= pcWire
	io.checkUnitIO.fetchReq2CheckUnit.ready			:= 
	readyReg & ((nextState === s_flow) & (state === s_flow))
	io.checkUnitIO.checkUnit2Sram.cacheBuf			:= axiDataVecReg
	io.checkUnitIO.checkUnit2Sram.tagBuf			:= tagWire
	io.checkUnitIO.checkUnit2Sram.replaceWay		:= replaceWay
	io.checkUnitIO.checkUnit2Sram.replaceIndex		:= indexWire
	io.checkUnitIO.checkUnit2Sram.wen				:= (state === s_load)
}

class PreDecoder extends Module {
	val io = IO(new Bundle {
		val preDecoderIO = new PreDecoderIO
	})

	val validReg = RegInit(0.B)
	val readyReg = RegInit(1.B)
	when(io.preDecoderIO.flush) {
		validReg := 0.B
		readyReg := 1.B
	} .otherwise {
		switch(validReg) {
			is(0.B) { validReg := io.preDecoderIO.checkUnit2PreDecoder.valid & io.preDecoderIO.checkUnit2PreDecoder.ready}
			is(1.B) {
				validReg := Mux(io.preDecoderIO.preDecoder2IFU.valid & io.preDecoderIO.preDecoder2IFU.ready,
				Mux(io.preDecoderIO.checkUnit2PreDecoder.valid & io.preDecoderIO.checkUnit2PreDecoder.ready, 1.B, 0.B), 1.B)
			}
		}
		switch(readyReg) {
			is(0.B) { readyReg := io.preDecoderIO.preDecoder2IFU.valid & io.preDecoderIO.preDecoder2IFU.ready }
			is(1.B) {
				readyReg := Mux(io.preDecoderIO.checkUnit2PreDecoder.valid & io.preDecoderIO.checkUnit2PreDecoder.ready, 
				Mux(io.preDecoderIO.preDecoder2IFU.valid & io.preDecoderIO.preDecoder2IFU.ready, 1.B, 0.B), 1.B)
			}
		}
	}

	io.preDecoderIO.checkUnit2PreDecoder.ready	:= readyReg
	io.preDecoderIO.preDecoder2IFU.valid		:= validReg & (!io.preDecoderIO.flush)
	io.preDecoderIO.preDecoder2IFU.bits 		:= io.preDecoderIO.checkUnit2PreDecoder.bits	
}

class LRU(way: Int, indexWidth: Int) extends Module {
	val io = IO(new Bundle {
		val update_entry	= Input(Bool())
		val update_index	= Input(UInt(indexWidth.W))
		val lru_index		= Output(UInt(indexWidth.W))
	})

	// 优先矩阵寄存器
	val matrix = RegInit(VecInit(Seq.fill(way)(0.U(way.W))))

	// 矩阵更新逻辑
	val matrixNext = Wire(Vec(way, UInt(way.W)))
	for (i <- 0 until way) {
		when(io.update_entry && (i.U === io.update_index)) {
			// 更新访问行
			matrixNext(i) := (~(1.U << i)) & ((1 << way).U - 1.U)
		}.otherwise {
			matrixNext(i) := matrix(i)
		}
	}

	// 寄存器更新
	for (i <- 0 until way) {
		matrix(i) := matrixNext(i)
	}

	// 查找 LRU
	val lruIndexNext = Wire(UInt(indexWidth.W))
	lruIndexNext := 0.U
	for (i <- 0 until way) {
		when(matrix(i) === 0.U) {
			lruIndexNext := i.U
		}
	}

	// 输出 LRU
	val lruIndexReg = RegNext(lruIndexNext, 0.U)
	io.lru_index := lruIndexReg
}

class FIFO(way: Int, indexWidth: Int) extends Module {
	val io = IO(new Bundle {
		val update_entry = Input(Bool())
		val update_index = Input(UInt(indexWidth.W)) // FIFO 不需要，用于接口统一可忽略
		val fifo_index   = Output(UInt(indexWidth.W))
	})

	// 轮转替换指针
	val fifo_ptr = RegInit(0.U(indexWidth.W))

	// 输出当前指针
	io.fifo_index := fifo_ptr

	// 每次更新后，指针后移（轮转）
	when(io.update_entry) {
		when(fifo_ptr === (way - 1).U) {
			fifo_ptr := 0.U
		}.otherwise {
			fifo_ptr := fifo_ptr + 1.U
		}
	}
}

class Random(way: Int, indexWidth: Int) extends Module {
	val io = IO(new Bundle {
		val update_entry  = Input(Bool())
		val update_index  = Input(UInt(indexWidth.W)) // 实际上不需要，用于接口统一
		val random_index  = Output(UInt(indexWidth.W))
	})

	// 伪随机数发生器 (LFSR)
	val lfsr = chisel3.util.random.LFSR(indexWidth)
	io.random_index := lfsr
}

class Replacement_Algorithm_Unit(way: Int, indexWidth: Int, policy: ReplacePolicy.Type) extends Module {
	val io = IO(new Bundle {
		val update_entry	= Input(Bool())
		val update_index	= Input(UInt(indexWidth.W))
		val index			= Output(UInt(indexWidth.W))
	})

	policy match {
		case ReplacePolicy.LRU => {
		// 实例化 LRU 管理逻辑
			val lru = Module(new LRU(way, indexWidth))
			lru.io.update_entry := io.update_entry
			lru.io.update_index := io.update_index
			io.index := lru.io.lru_index
		}

		case ReplacePolicy.FIFO => {
			val fifo = Module(new FIFO(way, indexWidth))
			fifo.io.update_entry := io.update_entry
			fifo.io.update_index := io.update_index
			io.index := fifo.io.fifo_index
		}

		case ReplacePolicy.RANDOM => {
			val random = Module(new Random(way, indexWidth))
			random.io.update_entry := io.update_entry
			random.io.update_index := io.update_index
			io.index := random.io.random_index
		}
	}
}

class Replacement_Algorithm(way: Int, sets: Int, indexWidth: Int, policy: ReplacePolicy.Type) extends Module {
	val io = IO(new Bundle {
		val update_entry	= Input(Bool())
		val update_index	= Input(UInt(indexWidth.W))
		val way_index		= Output(UInt(indexWidth.W))
	})

	val wayArray = Wire(Vec(sets, UInt(indexWidth.W)))
	val uints = Seq.fill(sets)(Module(new Replacement_Algorithm_Unit(way, indexWidth, policy)))

	for (i <- 0 until sets) {
		uints(i).io.update_entry := io.update_entry && (io.update_index === i.U)
		uints(i).io.update_index := io.update_index
		wayArray(i) := uints(i).io.index
	}

	io.way_index := wayArray(io.update_index)
}
