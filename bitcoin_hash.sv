module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

enum logic [4:0] {IDLE, PHASE_ONE_READ, PHASE_ONE_CALCULATE, PHASE_TWO_READ, PHASE_TWO_CALCULATE, PHASE_THREE_READ, PHASE_THREE_CALCULATE} state;
logic [31:0] hout[num_nonces];

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

// Student to add rest of the code here

logic [31:0] sha256_in[16]; 
logic [31:0] sha256_hash[8]; 
logic [31:0] sha256_out_1[8], sha256_out_2[8], sha256_out_3[8];
//logic [31:0] sha256_out_temp;
logic [7:0] count;
logic [15:0] cur_addr;
logic start_sha256_1, start_sha256_2, start_sha256_3;
logic sha256_done_1, sha256_done_2, sha256_done_3;
logic [31:0] starting_hash[8];
logic [31:0] final_out[256];

assign starting_hash[0] = 32'h6a09e667;
assign starting_hash[1] = 32'hbb67ae85;
assign starting_hash[2] = 32'h3c6ef372;
assign starting_hash[3] = 32'ha54ff53a;
assign starting_hash[4] = 32'h510e527f;
assign starting_hash[5] = 32'h9b05688c;
assign starting_hash[6] = 32'h1f83d9ab;
assign starting_hash[7] = 32'h5be0cd19;


always_ff @(posedge clk, negedge reset_n) begin
	if (reset_n == 0) begin
		count <= 0;
		start_sha256_1 <= 0;
		start_sha256_2 <= 0;
		start_sha256_3 <= 0;
		state <= PHASE_ONE_READ;
		for (int i = 0; i < 16; i++) begin
			sha256_in[i] <= 0;
		end
		for (int i = 0; i < 8; i++) begin
			sha256_hash[i] <= 0;
		end
		
	end
	else case(state)
		PHASE_ONE_READ : begin // This reads w[0] to w[15] into sha256_in vector, which is loaded into sha256 inst
			if (count == 0) begin
				cur_addr <= message_addr;
			end
			if (count < 16) begin
				sha256_in[count] <= mem_read_data;
				mem_addr <= cur_addr + 1;
				count <= count + 1;
				state <= PHASE_ONE_READ;
			end
			else begin
				state <= PHASE_ONE_CALCULATE;
				count <= 0;
			end
		end
		
		PHASE_ONE_CALCULATE : begin
			if (!start_sha256_1) begin
				start_sha256_1 <= 1;
				state <= PHASE_ONE_CALCULATE;
			end
			else if (!sha256_done_1) state <= PHASE_ONE_CALCULATE;
			else begin
				sha256_hash <= sha256_out_1; // writes phase 1 out to hash of phase 2
				state <= PHASE_TWO_READ;
			end
		end
			
		PHASE_TWO_READ : begin // handles inputs to phase 2 sha256
			if (count == 0) begin
				for (int i = 0; i < 3; i++) begin // write message[16 to 18] into sha256_in[0 to 2]
					mem_addr <= message_addr + i + 16; 
					sha256_in[i] <= mem_read_data;
				end
				for (int i = 4; i < 14; i++) begin // padding
					sha256_in[i] <= 0;
				end
				sha256_in[15] <= 32'd640; // message length
				state <= PHASE_TWO_CALCULATE;
			end
			sha256_in[3] <= count; // nonce increments by 1 each cycle			
		end
		
		PHASE_TWO_CALCULATE : begin
			if (!start_sha256_2) begin
				start_sha256_2 <= 1;
				state <= PHASE_TWO_CALCULATE;
			end
			else if (sha256_done_2 && count < 16) begin // repeat calculation 16 times
				
				state <= PHASE_TWO_READ;
			end
			else if (sha256_done_2) begin
				for (int i = 0; i < 8; i++) begin // write phase 2 out into input of phase 3 [0 to 7]
					sha256_in[i] <= sha256_out_2[i];
				end
				state <= PHASE_THREE_READ;
			end
			else state <= PHASE_TWO_CALCULATE;		
		end
		
		PHASE_THREE_READ : begin
			//TODO
			// reads 8 words outputted from phase 2, all other words are 0
			// will increment count up to 16, repeating phase two and three for each nonce value
			// At the end, write h0 from each iteration to memory
		end
		
		PHASE_THREE_CALCULATE : begin
			//TODO
		end
		
		default : begin
		 state <= IDLE;
		end
		
		endcase
	end

simplified_sha256_part2 sha_256_inst_phase1 ( // changed the sha256 file name to include _part2 for clarity
		.clk(clk),
		.reset_n(reset_n),
		.start(start_sha256_1),
		
		.message_addr(0),
		.output_addr(0),
		.done(sha256_done_1),
		.mem_clk(),
		.mem_we(),
		
		.mem_addr(),
		.mem_write_data(sha256_out_1), // writes out in parallel
		.mem_read_data(sha256_in), // reads in a vector in parallel
		.hash(starting_hash)
	);
	
simplified_sha256_part2 sha_256_inst_phase2 (
		.clk(clk),
		.reset_n(reset_n),
		.start(start_sha256_2),
		
		.message_addr(0),
		.output_addr(0),
		.done(sha256_done_2),
		.mem_clk(),
		.mem_we(),
		
		.mem_addr(),
		.mem_write_data(sha256_out_2), 
		.mem_read_data(sha256_in), 
		.hash(sha256_hash) // uses hash from phase 1 output
	);

simplified_sha256_part2 sha_256_inst_phase3 (
		.clk(clk),
		.reset_n(reset_n),
		.start(start_sha256_3),
		
		.message_addr(0),
		.output_addr(0),
		.done(sha256_done_3),
		.mem_clk(),
		.mem_we(),
		
		.mem_addr(),
		.mem_write_data(sha256_out_3), 
		.mem_read_data(sha256_in), 
		.hash(starting_hash) // uses original hash
	);

endmodule
