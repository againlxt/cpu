package alu

import chisel3._
import chisel3.util._

/* 
输入信号：

- **aluCtr**[3:0]：alu控制信号输入，000时做加减法，001时做左移位，010时做比较，011输入B的结果直接输出，100时选择异或，101时做算术和逻辑右移，110逻辑或输出，111逻辑与输出

输出信号：

- **aOrL**：A/L控制移位器进行算术移位还是逻辑移位，0为逻辑，1为算术
-  **lOrR**：L/R控制是左移还是右移，0为右移，1为左移
- **uOrS**：U/S控制比较大小是带符号比较还是无符号比较，0为无符号比较，1为带符号比较
- **subOrAdd**：S/A控制是加法还是减法，0为加法，1为减法。
*/
class ALUControl extends Module {
	val io = IO(new Bundle {
		val aluCtr 		= Input(UInt(4.W))
		
		val aOrLCtr 	= Output(UInt(1.W))
		val lOrRCtr 	= Output(UInt(1.W))
		val uOrSCtr		= Output(UInt(1.W))
		val subOrAddCtr	= Output(UInt(1.W))
	})

	val aluCtrWire = MuxCase (ALUMuxCtr.NOP, Seq(
		(io.aluCtr(2,0) === "b000".U) -> ALUMuxCtr.ADDER,
		(io.aluCtr(2,0) === "b001".U) -> ALUMuxCtr.LSHIFT,
		(io.aluCtr(2,0) === "b010".U) -> ALUMuxCtr.SLT,
		(io.aluCtr(2,0) === "b011".U) -> ALUMuxCtr.B,
		(io.aluCtr(2,0) === "b100".U) -> ALUMuxCtr.XOR,
		(io.aluCtr(2,0) === "b101".U) -> ALUMuxCtr.RSHIFT,
		(io.aluCtr(2,0) === "b110".U) -> ALUMuxCtr.OR,
		(io.aluCtr(2,0) === "b111".U) -> ALUMuxCtr.AND
	))
	val aOrLCtrWire = WireDefault(0.U(1.W))
	val lOrRCtrWire = WireDefault(0.U(1.W))
	val uOrSCtrWire = WireDefault(0.U(1.W))
	val subOrAddCtrWire = WireDefault(0.U(1.W))

	switch(aluCtrWire) {
		is(ALUMuxCtr.ADDER) {
			when(io.aluCtr(3) === 0.U) {
				aOrLCtrWire 	:= 0.U
				lOrRCtrWire 	:= 0.U
				uOrSCtrWire 	:= 0.U
				subOrAddCtrWire := 0.U
			}. otherwise {
				aOrLCtrWire 	:= 0.U
				lOrRCtrWire 	:= 0.U
				uOrSCtrWire 	:= 0.U
				subOrAddCtrWire := 1.U
			}
		}
		is(ALUMuxCtr.LSHIFT) {
			aOrLCtrWire 	:= 0.U
			lOrRCtrWire 	:= 1.U
			uOrSCtrWire 	:= 0.U
			subOrAddCtrWire := 0.U
		}
		is(ALUMuxCtr.SLT) {
			when(io.aluCtr(3) === 0.U) {
				aOrLCtrWire 	:= 0.U
				lOrRCtrWire 	:= 0.U
				uOrSCtrWire 	:= 1.U
				subOrAddCtrWire := 1.U
			}. otherwise {
				aOrLCtrWire 	:= 0.U
				lOrRCtrWire 	:= 0.U
				uOrSCtrWire 	:= 0.U
				subOrAddCtrWire := 1.U
			}
		}
		is(ALUMuxCtr.B) {
			aOrLCtrWire 	:= 0.U
			lOrRCtrWire 	:= 0.U
			uOrSCtrWire 	:= 0.U
			subOrAddCtrWire := 0.U
		}
		is(ALUMuxCtr.XOR) {
			aOrLCtrWire 	:= 0.U
			lOrRCtrWire 	:= 0.U
			uOrSCtrWire 	:= 0.U
			subOrAddCtrWire := 0.U
		}
		is(ALUMuxCtr.RSHIFT) {
			when(io.aluCtr(3) === 0.U) {
				aOrLCtrWire 	:= 0.U
				lOrRCtrWire 	:= 0.U
				uOrSCtrWire 	:= 0.U
				subOrAddCtrWire := 0.U
			}. otherwise {
				aOrLCtrWire 	:= 1.U
				lOrRCtrWire 	:= 0.U
				uOrSCtrWire 	:= 0.U
				subOrAddCtrWire := 0.U
			}
		}
		is(ALUMuxCtr.OR) {
			aOrLCtrWire 	:= 0.U
			lOrRCtrWire 	:= 0.U
			uOrSCtrWire 	:= 0.U
			subOrAddCtrWire := 0.U
		}
		is(ALUMuxCtr.AND) {
			aOrLCtrWire 	:= 0.U
			lOrRCtrWire 	:= 0.U
			uOrSCtrWire 	:= 0.U
			subOrAddCtrWire := 0.U
		}
		is(ALUMuxCtr.NOP) {
			aOrLCtrWire 	:= 0.U
			lOrRCtrWire 	:= 0.U
			uOrSCtrWire 	:= 0.U
			subOrAddCtrWire := 0.U
		}
	}

	io.aOrLCtr 		:= aOrLCtrWire
	io.lOrRCtr  	:= lOrRCtrWire
	io.uOrSCtr 		:= uOrSCtrWire
	io.subOrAddCtr 	:= subOrAddCtrWire
}

object ALUMuxCtr extends ChiselEnum {
	val ADDER, LSHIFT, SLT, B, XOR, RSHIFT, OR, AND, NOP = Value
}
