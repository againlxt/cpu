package basemode

import chisel3._
import chisel3.util._

class AXIAccessFault extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val valid   = Input(Bool())
        val ready   = Input(Bool())
        val resp    = Input(UInt(2.W))
    })

	setInline("AXIAccessFault.sv",
	"""module AXIAccessFault(
    |   input valid,
    |   input ready,
	|	input [1:0] resp
	|);
    |import "DPI-C" function void axi_access_fault(input byte resp);
	|always@(*) begin 
    |   if(resp != 0 || resp != 1 && (valid && ready)) axi_access_fault({6'd0, resp});
    |end
	|
	|endmodule
	""".stripMargin)
}
