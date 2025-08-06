package dpic

import chisel3._
import chisel3.util._

object PerformanceCounterType extends ChiselEnum {
    val OTHER, JUMP, STROE, LOAD, CAL, CSR, IFUGETINST, LSUGETDATA, EXUFINCAL,
	ICACHE_ACCESS_TIME, ICACHE_MISS_PENALTY, FLUSHCNT, RAWCNT = Value
}

class PerformanceCounter extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val valid           = Input(Bool())
        val counterType     = Input(UInt(32.W))
        val data            = Input(UInt(32.W))
    })

    setInline("PerformanceCounter.sv",
    """module PerformanceCounter(
	|	input valid,
	|	input [31:0] counterType,
	|	input [31:0] data
	|);
	|
	|import "DPI-C" function void performence_cnt_record(input int cnttype, input int data);
	|always @(posedge valid) begin
	|	performence_cnt_record(counterType, data);
	|end
	|endmodule
    """.stripMargin)
}
