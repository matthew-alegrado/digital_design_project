module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

enum logic [4:0] {PHASE_ONE_READ, PHASE_ONE_CALCULATE, PHASE_TWO, PHASE_THREE} state;
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
logic [31:0] sha256_out[8];
logic [31:0] sha256_out_temp;
logic [7:0] count;
logic [15:0] cur_addr;
//logic done;

always_ff @(posedge clk, negedge reset_n) begin
	if (reset_n == 0) begin
		count <= 0;
		done <= 0;
		for (int i = 0; i < 16; i++) begin
			sha256_in[i] <= 0;
		end
		for (int i = 0; i < 8; i++) begin
			sha256_out[i] <= 0;
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
			end
		end
		
		PHASE_ONE_CALCULATE : begin
			if (sha256_out[0] == 0) begin
				count <= 0;
				state <= PHASE_ONE_CALCULATE;
			end
			else if (count < 8 && done) begin
				sha256_out[count] <= sha256_out_temp;
				state <= PHASE_ONE_CALCULATE;
			end
			else if (done) state <= PHASE_TWO;
			else state <= PHASE_ONE_CALCULATE;
		end
			

		
			
		
		
		
		endcase
	end

simplified_sha256_part2 sha_256_inst ( // changed the sha256 file name to include _part2 for clarity
		.clk(clk),
		.reset(reset_n),
		.start(start),
		
		//.message_addr(0),
		//.output_addr(0),
		.done(done),
		.mem_clk(mem_clk),
		.mem_we(mem_we),
		
		//.mem_addr(),
		.mem_write_data(sha256_out_temp), // writes out individual words in series
		.mem_read_data(mem_read_data) // reads in a vector in parallel
	);
	



endmodule
