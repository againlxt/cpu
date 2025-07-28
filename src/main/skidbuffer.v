module SkidBuffer
#(
    parameter DATA_WIDTH = 8
)
(
    input   wire                        clock,
    input   wire                        reset,
    input   wire                        flush,

    input   wire                        input_valid,
    output  wire                        input_ready,
    input   wire    [DATA_WIDTH-1:0]    input_data,

    output  wire                        output_valid,
    input   wire                        output_ready,
    output  wire    [DATA_WIDTH-1:0]    output_data
);

/* The A register */
reg [DATA_WIDTH-1:0] a_data_q;
reg a_full_q;
wire a_fill, a_drain;
always @(posedge clock) begin
    if(reset)       a_data_q <= '0;
    else if(a_fill) a_data_q <= input_data;
end
always @(posedge clock) begin
    if(reset)       a_full_q <= '0;
    else if(a_fill || a_drain)
        a_full_q <= a_fill;
end
/* Fill the A register when the A or B register is empty. Drain the A register
 whenever it is full and being filled, or if a flush is requested. */
assign a_fill = input_valid && input_ready && (!flush);
assign a_drain = (a_full_q && !b_full_q) || flush;

/* The B register */
reg [DATA_WIDTH-1:0] b_data_q;
reg b_full_q;
wire b_fill, b_drain;
always @(posedge clock) begin
    if(reset)       b_data_q <= '0;
    else if(b_fill) b_data_q <= a_data_q;
end
always @(posedge clock) begin
    if(reset)       b_full_q <= '0;
    else if(b_fill || b_drain)
        b_full_q <= b_fill;
end
/* Fill the B register whenever the A register is drained, but the downstream
 circuit is not ready. Drain the B register whenever it is full and the
 downstream circuit is ready, or if a flush is requested. */
assign b_fill = a_drain && (!output_ready) && (!flush);
assign b_drain = (b_full_q && output_ready) || flush;

assign input_ready  = !a_full_q || !b_full_q;
assign output_valid = a_full_q | b_full_q;
assign output_data  = b_full_q ? b_data_q : a_data_q;
endmodule
