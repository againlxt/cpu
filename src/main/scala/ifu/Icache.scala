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
import os.group.set

/**
  * @param numOfCaches	:Number of caches
  * @param sizeOfCache	:Size of cache(bit)
  * @param m			:sizeOfCache/8 = 2^m
  * @param n			:numOfCaches = 2^n
  * @param burstLen		:AXI size
  * @param burstSize	:burst size(byte)
  */
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
	val pcReg 	 = RegEnable(io.fetchReqIO.ifu2FetchReq.bits.pc, 
	io.fetchReqIO.ifu2FetchReq.valid & io.fetchReqIO.ifu2FetchReq.ready)
	when(io.fetchReqIO.flush) {
		validReg := 0.B
	} .otherwise {
		switch(validReg) {
			is(0.B) { validReg := io.fetchReqIO.ifu2FetchReq.valid & io.fetchReqIO.ifu2FetchReq.ready}
		}
	}

	io.fetchReqIO.ifu2FetchReq.ready 		:= io.fetchReqIO.fetchReq2CheckUnit.ready || !validReg
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
	val tagWire 			= pcWire(31, m+n)
	val indexWire 			= pcWire(m+n-1, m)
	val offsetWire 			= pcWire(m-1,0) >> 2

	val cacheValidReg	= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(false.B)))))
	val cacheLineRegVec = io.checkUnitIO.checkUnit2Sram.cacheLineVec
	val wayTagRegVec 	= io.checkUnitIO.checkUnit2Sram.tagVec
	val wayValidVec 	= cacheValidReg(indexWire)
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
	val ra 				= Module(new Replacement_Algorithm(way, numOfCache/way, policy))
	ra.io.hit			:= (hitWire & feq2CheckHandReg)
	ra.io.hitway		:= hitWay
	ra.io.replaceEn		:= (state === s_load)
	ra.io.index			:= indexWire
	val replaceWay		= ra.io.replaceWay(indexWire)
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
	when (io.checkUnitIO.flush) {
		validReg := 0.B
	} .otherwise {
		switch(validReg) {
			is(0.B) { validReg := io.checkUnitIO.fetchReq2CheckUnit.valid & io.checkUnitIO.fetchReq2CheckUnit.ready}
			is(1.B) {
				validReg := Mux(io.checkUnitIO.checkUnit2PreDecoder.valid & io.checkUnitIO.checkUnit2PreDecoder.ready,
				Mux(io.checkUnitIO.fetchReq2CheckUnit.valid & io.checkUnitIO.fetchReq2CheckUnit.ready, 1.B, 0.B), 1.B)
			}
		}
	}

		/* Counter */
	if (Config.hasPerformanceCounter & (!Config.isSTA)) {
		val missPenaltyCounter	= RegInit(0.U(32.W))
		switch(nextState) {
			is(s_flow) {
				missPenaltyCounter := 0.U
			}
			is(s_miss) {
                missPenaltyCounter := missPenaltyCounter + 1.U
			}
		}
		
		val ATC 			= Module(new PerformanceCounter)
		ATC.io.valid		:= (state === s_flow) & (hitWire) & feq2CheckHandReg
		ATC.io.counterType	:= PerformanceCounterType.ICACHE_ACCESS_TIME.asUInt
		ATC.io.data 		:= 1.U
		val MPC 			= Module(new PerformanceCounter)
		MPC.io.valid		:= (state === s_miss) & findEndWire
		MPC.io.counterType	:= PerformanceCounterType.ICACHE_MISS_PENALTY.asUInt
		MPC.io.data 		:= missPenaltyCounter
	}

	io.checkUnitIO.checkUnit2PreDecoder.valid 		:= validReg & 
	((hitWire & (state === s_flow)) | ((state =/= s_flow) & axiHitVecReg(offsetWire))) & 
	(!io.checkUnitIO.flush)
	io.checkUnitIO.checkUnit2PreDecoder.bits.inst	:= 
	Mux((state === s_flow), cacheLineRegVec(hitWay)(offsetWire), axiDataVecReg(offsetWire))
	io.checkUnitIO.checkUnit2PreDecoder.bits.pc		:= pcWire
	io.checkUnitIO.fetchReq2CheckUnit.ready			:= 
	(io.checkUnitIO.checkUnit2PreDecoder.ready || !validReg) & ((nextState === s_flow) & (state === s_flow))
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
	when(io.preDecoderIO.flush) {
		validReg := 0.B
	} .otherwise {
		switch(validReg) {
			is(0.B) { validReg := io.preDecoderIO.checkUnit2PreDecoder.valid & io.preDecoderIO.checkUnit2PreDecoder.ready}
			is(1.B) {
				validReg := Mux(io.preDecoderIO.preDecoder2IFU.valid & io.preDecoderIO.preDecoder2IFU.ready,
				Mux(io.preDecoderIO.checkUnit2PreDecoder.valid & io.preDecoderIO.checkUnit2PreDecoder.ready, 1.B, 0.B), 1.B)
			}
		}
	}

	io.preDecoderIO.checkUnit2PreDecoder.ready	:= io.preDecoderIO.preDecoder2IFU.ready || !validReg
	io.preDecoderIO.preDecoder2IFU.valid		:= validReg & (!io.preDecoderIO.flush)
	io.preDecoderIO.preDecoder2IFU.bits 		:= io.preDecoderIO.checkUnit2PreDecoder.bits	
}

class LRUBitScheme(way: Int) extends Module {
    require(way > 0, "Number of ways must be positive")
    
    val io = IO(new Bundle {
        val replaceEn    = Input(Bool())
        val hitWay       = Input(UInt(log2Up(way).W))
        val hit          = Input(Bool())
        val lru_index    = Output(UInt(log2Up(way).W))
    })

    // 优先矩阵寄存器 - 初始化全0
    val matrix = RegInit(VecInit(Seq.fill(way)(VecInit(Seq.fill(way)(false.B)))))

    // 正确的矩阵更新逻辑
    when(io.hit || io.replaceEn) {
        val hitWayIdx = io.hitWay
        
        // 更新被访问way的行和列
        for (i <- 0 until way) {
            // 设置被访问way的行：所有位设为1（表示它比所有其他way都新）
            when(i.U === hitWayIdx) {
                for (j <- 0 until way) {
                    matrix(i)(j) := (j.U =/= hitWayIdx) // 对角线保持0
                }
            }.otherwise {
                // 设置其他way的对应列：设为0（表示它们比被访问way旧）
                matrix(i)(hitWayIdx) := false.B
                // 设置被访问way的对应列：设为1
                matrix(hitWayIdx)(i) := true.B
            }
        }
    }

    // 正确的LRU查找：找到全0的行（最久未使用）
    val lruCandidates = Wire(Vec(way, Bool()))
    for (i <- 0 until way) {
        // 检查第i行是否全为0（除了对角线）
        val isAllZero = (0 until way).map(j => 
            if (i == j) true.B else !matrix(i)(j)
        ).reduce(_ && _)
        lruCandidates(i) := isAllZero
    }

    // 优先级编码器选择LRU
    io.lru_index := PriorityEncoder(lruCandidates)
}

class FIFO(way: Int) extends Module {
	val io = IO(new Bundle {
		val replaceEn = Input(Bool())
		val fifo_index   = Output(UInt(log2Up(way).W))
	})

	// 轮转替换指针
	val fifo_ptr = RegInit(0.U(log2Up(way).W))

	// 输出当前指针
	io.fifo_index := fifo_ptr

	// 每次更新后，指针后移（轮转）
	when(io.replaceEn) {
		when(fifo_ptr === (way - 1).U) {
			fifo_ptr := 0.U
		}.otherwise {
			fifo_ptr := fifo_ptr + 1.U
		}
	}
}

class Random(way: Int) extends Module {
	val io = IO(new Bundle {
		val replaceEn  	= Input(Bool())
		val random_index= Output(UInt(log2Up(way).W))
	})

	// 伪随机数发生器 (LFSR)
	val lfsr = RegEnable(chisel3.util.random.LFSR(log2Up(way)), io.replaceEn)
	io.random_index := lfsr
}

class Replacement_Algorithm_Unit(way: Int, policy: ReplacePolicy.Type) extends Module {
	val io = IO(new Bundle {
		val replaceEn 		= Input(Bool())
		val hit 			= Input(Bool())
		val hitway 			= Input(UInt((log2Up(way)).W))
		val replaceWay		= Output(UInt((log2Up(way)).W))
	})

	policy match {
		case ReplacePolicy.BSLRU => {
		// 实例化 LRU 管理逻辑
			val lru = Module(new LRUBitScheme(way))
			lru.io.replaceEn 	:= io.replaceEn
			lru.io.hit 			:= io.hit
			lru.io.hitWay		:= io.hitway
			io.replaceWay 		:= lru.io.lru_index
		}

		case ReplacePolicy.FIFO => {
			val fifo = Module(new FIFO(way))
			fifo.io.replaceEn 	:= io.replaceEn
			io.replaceWay 		:= fifo.io.fifo_index
		}

		case ReplacePolicy.RANDOM => {
			val random = Module(new Random(way))
			random.io.replaceEn 	:= io.replaceEn
			io.replaceWay 			:= random.io.random_index
		}
	}
}

class Replacement_Algorithm(way: Int, sets: Int, policy: ReplacePolicy.Type) extends Module {
	val io = IO(new Bundle {
		val replaceEn 		= Input(Bool())
		val index 			= Input(UInt((log2Up(sets)).W))
		val hit 			= Input(Bool())
		val hitway 			= Input(UInt((log2Up(way)).W))
		val replaceWay		= Output(Vec(sets, UInt((log2Up(way)).W)))
	})

	val wayArray = Wire(Vec(sets, UInt((log2Up(way)).W)))
	val uints = Seq.fill(sets)(Module(new Replacement_Algorithm_Unit(way, policy)))

	for (i <- 0 until sets) {
		uints(i).io.replaceEn	:= io.replaceEn & (io.index === i.U)
		uints(i).io.hit			:= io.hit
		uints(i).io.hitway 		:= io.hitway
		wayArray(i) 			:= uints(i).io.replaceWay
	}

	io.replaceWay := wayArray
}
