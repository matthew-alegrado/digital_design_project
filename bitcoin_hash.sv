module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;
parameter NUM_SHA256 = 8;

enum logic [2:0] {IDLE, INIT_READ, PHASE_ONE_READ, PHASE_ONE_CALCULATE, PHASE_TWO_READ, PHASE_TWO_CALCULATE, PHASE_THREE, FINAL_PHASE} state;


parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

parameter int starting_hash[8] = '{
	32'h6a09e667,
	32'hbb67ae85,
	32'h3c6ef372,
	32'ha54ff53a,
	32'h510e527f,
	32'h9b05688c,
	32'h1f83d9ab,
	32'h5be0cd19
};

// Student to add rest of the code here

logic [31:0] message[20];
logic [31:0] sha256_in[NUM_SHA256][16]; 
logic [31:0] sha256_hash[8]; 
logic [31:0] sha256_out[NUM_SHA256][8];
logic [31:0] cur_write_data;
logic [7:0] count, phase_iter;
logic delay_tmp;
logic [15:0] cur_addr;
logic start_sha256, start_sha256_parallel;
logic sha256_done[NUM_SHA256];
logic [31:0] final_out_h0[num_nonces], temp_out_h0[NUM_SHA256];
logic [31:0] phase_two_hash[8];
logic cur_we;

assign mem_write_data = cur_write_data;
assign mem_addr = cur_addr;
assign mem_clk = clk;
assign mem_we = cur_we;

always_ff @(posedge clk, negedge reset_n) begin
	if (!reset_n) begin
		delay_tmp <= 0;
		count <= 0;
		phase_iter <= 0;
		start_sha256 <= 0;
		start_sha256_parallel <= 0;
		cur_we <= 0;
		for (int j = 0; j < NUM_SHA256; j++) begin
			for (int i = 0; i < 16; i++) begin
				sha256_in[j][i] <= 0;
			end
		end
		for (int i = 0; i < 8; i++) begin
			sha256_hash[i] <= 0;
		end
		state <= IDLE;
	end
	else begin	
		case(state)
			IDLE : begin
				if (start) begin
					state <= INIT_READ;
					cur_addr <= message_addr;
				end
				else state <= IDLE;
			end
			
			INIT_READ : begin
				if (!delay_tmp) begin
					delay_tmp <= 1;
					cur_addr <= cur_addr + 1;
					state <= INIT_READ;
				end
				else begin
					message[count] <= mem_read_data;
					count <= count + 1;
					cur_addr <= cur_addr + 1;
					if (count < 20) state <= INIT_READ;
					else begin
						state <= PHASE_ONE_READ;
						delay_tmp <= 0;
					end
				end
			end
		
			PHASE_ONE_READ : begin // This reads w[0] to w[15] into sha256_in vector, which is loaded into sha256 inst
				for (int i = 0; i < 16; i++) begin
					sha256_in[0][i] <= message[i];
				end
				for (int i = 0; i < 8; i++) begin
					sha256_hash[i] <= starting_hash[i];
				end
				state <= PHASE_ONE_CALCULATE;
				
			end
			
			PHASE_ONE_CALCULATE : begin
				if (!delay_tmp && !start_sha256) begin
					start_sha256 <= 1;					
					state <= PHASE_ONE_CALCULATE;
				end
				else if (!delay_tmp) begin
					delay_tmp <= 1;
					state <= PHASE_ONE_CALCULATE;
				end
				else if (start_sha256) begin
					start_sha256 <= 0;
					state <= PHASE_ONE_CALCULATE;
				end
				else if (!sha256_done[0]) state <= PHASE_ONE_CALCULATE;
				else begin
					for (int i = 0; i < 8; i++) begin
						phase_two_hash[i] <= sha256_out[0][i];
					end
					state <= PHASE_TWO_READ;
					delay_tmp <= 0;
				end
			end
				
			PHASE_TWO_READ : begin // handles inputs to phase 2 sha256
				sha256_hash <= phase_two_hash;
				for (int j = 0; j < NUM_SHA256; j++) begin
					for (int i = 0; i < 3; i++) begin
						sha256_in[j][i] <= message[i + 16];
					end
					for (int i = 5; i < 15; i++) begin // padding
						sha256_in[j][i] <= 0;
					end
					sha256_in[j][3] <= phase_iter*NUM_SHA256 + j; // nonce increment
					sha256_in[j][4] <= 32'h80000000;
					sha256_in[j][15] <= 32'd640; // message length
					state <= PHASE_TWO_CALCULATE;	
				end
			end
			
			PHASE_TWO_CALCULATE : begin
				if (!start_sha256 && !delay_tmp) begin
					start_sha256 <= 1;
					start_sha256_parallel <= 1;
					state <= PHASE_TWO_CALCULATE;
				end
				else if (!delay_tmp) begin
					delay_tmp <= 1;
					state <= PHASE_TWO_CALCULATE;
				end
				else if (start_sha256) begin
					start_sha256 <= 0;
					start_sha256_parallel <= 0;
					state <= PHASE_TWO_CALCULATE;
				end
				else if (sha256_done[0] && sha256_done[NUM_SHA256 - 1]) begin
					for (int j = 0; j < NUM_SHA256; j++) begin
						for (int i = 0; i < 8; i++) begin // write phase 2 out into input of phase 3 [0 to 7]
							sha256_in[j][i] <= sha256_out[j][i];
							sha256_hash[i] <= starting_hash[i];
						end
						for (int i = 9; i < 15; i++) begin // phase 3 in [8 to 15] are 0s
							sha256_in[j][i] <= 0;
						end
						sha256_in[j][8] <= 32'h80000000;
						sha256_in[j][15] <= 32'd256;				
					end
					delay_tmp <= 0;
					state <= PHASE_THREE;
				end
				else state <= PHASE_TWO_CALCULATE;		
			end
			
			PHASE_THREE : begin
				//TODO
				// reads 8 words outputted from phase 2, all other words are 0
				// will increment count up to 16, repeating phase two and three for each nonce value
				// At the end, write h0 from each iteration to memory
				if (!start_sha256 && !delay_tmp) begin
					start_sha256 <= 1;
					start_sha256_parallel <= 1;
					state <= PHASE_THREE;
				end
				else if (!delay_tmp) begin
					delay_tmp <= 1;
					state <= PHASE_THREE;
				end
				else if (start_sha256) begin
					start_sha256 <= 0;
					start_sha256_parallel <= 0;
					state <= PHASE_THREE;
				end 
				else if (sha256_done[0] && sha256_done[NUM_SHA256 - 1] && delay_tmp) begin
					for (int j = 0; j < NUM_SHA256; j++) begin
						final_out_h0[phase_iter*NUM_SHA256 + j] <= sha256_out[j][0];						
					end
					state <= FINAL_PHASE;
					count <= 0;
					delay_tmp <= 0;
					cur_we <= 1;
				end
				else begin 
					state <= PHASE_THREE;
					delay_tmp <= 1;
				end
			end
			
			FINAL_PHASE : begin	
				if (phase_iter*NUM_SHA256 < num_nonces) begin
					if (!delay_tmp) begin
						state <= FINAL_PHASE;
						cur_we <= 1;
						delay_tmp <= 1;
					end
					else if (count < NUM_SHA256) begin
						cur_write_data <= final_out_h0[phase_iter*NUM_SHA256 + count];
						cur_addr <= output_addr + phase_iter*NUM_SHA256 + count;
						state <= FINAL_PHASE;
						count <= count + 1;
					end
					else begin
						phase_iter <= phase_iter + 1;
						cur_we <= 1;
						state <= PHASE_TWO_READ;
						delay_tmp <= 0;
					end
				end 
				else begin
					state <= IDLE;
					cur_we <= 0;
				end
			end
			
			default : begin
			 state <= IDLE;
			end
		endcase
	end
end
	
assign done = (state == IDLE);

simplified_sha256_part2 sha_256_inst_1 ( // changed the sha256 file name to include _part2 for clarity
		.clk(clk),
		.reset_n(reset_n),
		.start(start_sha256),
		.done(sha256_done[0]),

		.mem_write_data(sha256_out[0]), // writes out in parallel
		.mem_read_data(sha256_in[0]), // reads in a vector in parallel
		.hash(sha256_hash)
	);
	
genvar t;	
generate
	// this will generate all of the other sha256 modules besides the first one
	for (t = 1; t < NUM_SHA256; t++) begin : sha256_gen
		simplified_sha256_part2 sha_256_inst (
			.clk(clk),
			.reset_n(reset_n),
			.start(start_sha256_parallel),
			.done(sha256_done[t]),
	
			.mem_write_data(sha256_out[t]), // writes out in parallel
			.mem_read_data(sha256_in[t]), 
			.hash(sha256_hash)
		);
	end

endgenerate

endmodule
