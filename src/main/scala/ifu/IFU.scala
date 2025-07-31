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
object ReplacePolicy extends ChiselEnum {
  val LRU, FIFO, RANDOM = Value
}
class IFU extends Module {
    val io = IO(new Bundle {
        val inst     	= Decoupled(new IFU2IDU)
		val ifu2Mem		= new AXI
		val wbu2IFU     = Flipped(Decoupled(new WBU2IFU))
		val wbu2Icache	= Input(Bool())
    })
	
	val pcReg 		= RegInit(Mux(Config.SoC.asBool, "h30000000".U(32.W), "h80000000".U(32.W)))
	val pcValidReg	= RegNext(io.wbu2IFU.valid & io.wbu2IFU.ready, 1.B)
	val ifuReadyReg	= RegInit(0.B)
	switch(ifuReadyReg) {
		is(0.B) {ifuReadyReg := io.inst.valid & io.inst.ready}
		is(1.B) {ifuReadyReg := !(io.wbu2IFU.valid & io.wbu2IFU.ready)}
	}
	pcReg := Mux(io.wbu2IFU.valid & io.wbu2IFU.ready, io.wbu2IFU.bits.nextPC, pcReg)

	val numOfCache 	= 16
	val sizeOfCache	= 128
	val way 		= 4
	val m 			= log2Ceil(sizeOfCache >> 3)
	val n 			= log2Up(numOfCache/way)
	val burstLen	= 4
	val burstSize 	= 16
	val icache = Module(new Icache(numOfCache, sizeOfCache, m, n, burstLen, burstSize, way, ReplacePolicy.LRU))
	icache.io.addr 		:= pcReg 
	icache.io.enable	:= pcValidReg 
	icache.io.icache2Mem <> io.ifu2Mem
	icache.io.wbu2Icache:= io.wbu2Icache

	if(Config.hasDPIC & (!Config.isSTA)) {
		val axiAccessFault = Module(new AXIAccessFault())
		axiAccessFault.io.ready := io.ifu2Mem.bready
		axiAccessFault.io.valid := io.ifu2Mem.bready
		axiAccessFault.io.resp	:= io.ifu2Mem.bresp
	}
	if(!Config.isSTA) {
		val pcUpdate		= Module(new PCUpdate)
		val pcUpdateWire 	= io.wbu2IFU.ready && io.wbu2IFU.valid
		pcUpdate.io.valid	:= pcUpdateWire
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
		IGIC.io.valid		:= io.ifu2Mem.rvalid && io.ifu2Mem.rready && io.ifu2Mem.rlast
		IGIC.io.counterType	:= PerformanceCounterType.IFUGETINST.asUInt
		IGIC.io.data 		:= ifuGetInstCounter
	}

	io.inst.valid		:= icache.io.oEnable
	io.inst.bits.inst	:= icache.io.inst
	io.inst.bits.pc		:= pcReg
	io.wbu2IFU.ready	:= ifuReadyReg
}

class PCUpdate extends BlackBox with HasBlackBoxInline {
	val io = IO(new Bundle{
		val valid	= Input(Bool())
	})

	setInline("PCUpdate.sv",
	"""module PCUpdate(
	|	input valid
	|);
	|export "DPI-C" function pcUpdate;
	|function bit pcUpdate;
	|	return valid;
	|endfunction
	|endmodule
	""".stripMargin)
}

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
        val addr        = Input(UInt(32.W))
        val enable      = Input(Bool())
        val inst        = Output(UInt(32.W))
        val oEnable     = Output(Bool())
        val icache2Mem  = new AXI
		val wbu2Icache	= Input(Bool())
    })
	val cacheValidReg 	= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(false.B)))))
	val tagReg   		= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(0.U((32-m-n).W))))))
	val cache  			= RegInit(VecInit(Seq.fill(numOfCache/way)(VecInit(Seq.fill(way)(VecInit(Seq.fill(burstSize >> 2)(0.U(32.W))))))))

    val addrReg         = RegInit(0.U(32.W))
	val tagWire			= addrReg(31, m+n)
	val indexWire 		= addrReg(m+n-1, m)
	val offsetWire 		= addrReg(m-1,0) >> 2
	val wayValidVec 	= cacheValidReg(indexWire)
	val wayTagVec		= tagReg(indexWire)
	val hitVec 			= wayValidVec.zip(wayTagVec).map { case (v, t) => v && (t === tagWire) }
	val hitWire     	= hitVec.reduce(_ || _)
	val hitWay 			= PriorityEncoder(hitVec)

	val sets 			= numOfCache/way
	val wayIndexWidth 	= log2Up(way)
	val replacement_algorithm	= Module(new Replacement_Algorithm
	(way, sets, n, n, policy))
	replacement_algorithm.io.update_entry 	:= hitWire
	replacement_algorithm.io.update_index	:= indexWire
	replacement_algorithm.io.set_index		:= wayIndexWidth.asUInt
	val wayIndex 		= replacement_algorithm.io.way_index

    val s_idle   = "b00001".U
    val s_check  = "b00010".U
    val s_find   = "b00100".U
	val s_find_b = "b01000".U
    val s_output = "b10000".U
    val state       = RegInit(1.U(5.W))
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
    io.inst    := cache(addrReg(m+n-1, m))(wayIndex)(offsetWire)
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

class Replacement_Algorithm(way: Int, sets: Int, indexWidth: Int, setIndexWidth: Int, policy: ReplacePolicy.Type) extends Module {
	val io = IO(new Bundle {
		val update_entry	= Input(Bool())
		val update_index	= Input(UInt(indexWidth.W))
		val set_index		= Input(UInt(setIndexWidth.W))
		val way_index		= Output(UInt(indexWidth.W))
	})

	val wayArray 	= WireDefault(VecInit(Seq.fill(sets)(0.U(indexWidth.W))))
	val unitArray 	= Seq.tabulate(sets) { i =>
		val uint 				= Module(new Replacement_Algorithm_Unit(way, indexWidth, policy))
		uint.io.update_entry	:= io.update_entry & (io.set_index === i.asUInt)
		uint.io.update_index	:= io.update_index
		io.way_index			:= wayArray(setIndexWidth) 
	}

	io.way_index := wayArray(io.set_index)
}
