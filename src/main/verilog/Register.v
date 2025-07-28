module Register #(
    parameter DATA_WIDTH = 32,               // 数据位宽参数，默认32位
    parameter RESET_VALUE = 0                // 复位值参数，默认0
) (
    input wire clock,                        // 时钟信号
    input wire clock_enable,                 // 时钟使能信号
    input wire clear,                        // 同步清零信号
    input wire [DATA_WIDTH-1:0] data_in,     // 输入数据
    output reg [DATA_WIDTH-1:0] data_out     // 输出数据
);

// 寄存器行为描述
always @(posedge clock) begin
    if (clear) begin                         // 同步清零优先级最高
        data_out <= RESET_VALUE;
    end else if (clock_enable) begin         // 当时钟使能有效时更新数据
        data_out <= data_in;
    end else begin
        data_out <= data_out;
    end
    // 如果没有时钟使能，则保持当前值
end

endmodule
