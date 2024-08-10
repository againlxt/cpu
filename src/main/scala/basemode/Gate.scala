package gate

import chisel3._
import chisel3.util._

class AND(dataWidth: Int) extends Module {
	val io = IO(new Bundle {
		val in1 = Input(UInt(dataWidth.W))
		val in2 = Input(UInt(dataWidth.W))
		val out = Output(UInt(dataWidth.W))
	})

	io.out 	:= io.in1 & io.in2
}

class OR(dataWidth: Int) extends Module {
	val io = IO(new Bundle {
		val in1 = Input(UInt(dataWidth.W))
		val in2 = Input(UInt(dataWidth.W))
		val out = Output(UInt(dataWidth.W))
	})

	io.out 	:= io.in1 | io.in2
}

class NOT(dataWidth: Int) extends Module {
	val io = IO(new Bundle {
		val in  = Input(UInt(dataWidth.W))
		val out = Output(UInt(dataWidth.W))
	})

	io.out 	:= ~io.in
}

class XOR(dataWidth: Int) extends Module {
	val io = IO(new Bundle {
		val in1 = Input(UInt(dataWidth.W))
		val in2 = Input(UInt(dataWidth.W))
		val out = Output(UInt(dataWidth.W))
	})

	io.out 	:= io.in1 ^ io.in2
}

class NAND(dataWidth: Int) extends Module {
	val io = IO(new Bundle {
		val in1 = Input(UInt(dataWidth.W))
		val in2 = Input(UInt(dataWidth.W))
		val out = Output(UInt(dataWidth.W))
	})

	io.out 	:= ~(io.in1 & io.in2)
}

class NOR(dataWidth: Int) extends Module {
	val io = IO(new Bundle {
		val in1 = Input(UInt(dataWidth.W))
		val in2 = Input(UInt(dataWidth.W))
		val out = Output(UInt(dataWidth.W))
	})

	io.out 	:= ~(io.in1 | io.in2)
}

class XNOR(dataWidth: Int) extends Module {
	val io = IO(new Bundle {
		val in1 = Input(UInt(dataWidth.W))
		val in2 = Input(UInt(dataWidth.W))
		val out = Output(UInt(dataWidth.W))
	})

	io.out 	:= ~(io.in1 ^ io.in2)
}

