module simplified_sha256_part2 #(
	parameter integer NUM_OF_WORDS = 16,
							H0_P = 32'h6a09e667, // h values for phase 1
							H1_P = 32'hbb67ae85,
							H2_P = 32'h3c6ef372,
							H3_P = 32'ha54ff53a,
							H4_P = 32'h510e527f,
							H5_P = 32'h9b05688c,
							H6_P = 32'h1f83d9ab,
							H7_P = 32'h5be0cd19
	)
(input logic  clk, reset_n, start,
 //input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 //output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data[16]); // changed mem_read_data to a vector, meant to connect to sha256_in

// FSM state variables 
enum logic [2:0] {IDLE, READ, BLOCK, COMPUTE, WRITE} state;

// Local variables
logic [31:0] w[64];
logic [31:0] message[16]; // changed from 20 to 16, should only be taking in 16 words at a time
logic [31:0] wt;
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [ 7:0] i, j;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic [7:0]  curr_block;
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
logic [512:0] memory_block;
logic [ 7:0] tstep;

// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


assign num_blocks = 1; // should only support one block
assign tstep = (i - 1);
assign wt = w[i];

// Function to determine number of blocks in memory to fetch
function logic [15:0] determine_num_blocks(input logic [31:0] size);

  determine_num_blocks = size / 512 + (size % 512 != 0);

endfunction


// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    // Student to add remaning code below
    // Refer to SHA256 discussion slides to get logic for this function
    ch = (e & f) ^ (~e & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = rightrotate(a,2) ^ rightrotate(a,13) ^ rightrotate(a,22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
//assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;


// Right rotation function
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
   rightrotate = (x >> r) | (x << (32 - r));
endfunction


// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
  end 
  else case (state)
    // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
    IDLE: begin 
       if(start) begin
			h0 <= H0_P; // starting h seed has been parametrized, so we can modify it from outside
			h1 <= H1_P;
			h2 <= H2_P;
			h3 <= H3_P;
			h4 <= H4_P;
			h5 <= H5_P;
			h6 <= H6_P;
			h7 <= H7_P;
			//cur_addr <= message_addr;
			offset <= 0;
			cur_we <= 0;
			curr_block <= 0;
			state <= READ;
       end else begin
			state <= IDLE;
		 end
    end
	 
	 READ: begin
        /*if (offset == 0) begin
            cur_addr <= message_addr;
        end
        message[offset] <= mem_read_data;
        offset <= offset + 1;
        if (offset == NUM_OF_WORDS) state <= BLOCK;
        else state <= READ;*/
		  message <= mem_read_data;
     end

    // SHA-256 FSM 
    // Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
    // and write back hash value back to memory
    BLOCK: begin
	// Fetch message in 512-bit block size
	// For each of 512-bit block initiate hash value computation
			if (curr_block == num_blocks) begin
				offset <= 0;
				state <= WRITE;
				cur_we <= 1;
			end else begin
				for (int l = 0; l < 16; l++) begin
					if ((curr_block << 4) + l >= NUM_OF_WORDS) begin
						if (l == 15)
							w[l] <= (NUM_OF_WORDS << 5);
						else if (l == 14)
							w[l] <= (NUM_OF_WORDS >> 27);
						else if ((curr_block << 4) + l == NUM_OF_WORDS)
							w[l] <= {1'b1,31'b0};
						else
							w[l] <= 32'b0;
					end else
						w[l] <= message[(curr_block << 4) + l];
				end
				a = h0;
				b = h1;
				c = h2;
				d = h3;
				e = h4;
				f = h5;
				g = h6;
				h = h7;
				i <= 0;
				state <= COMPUTE;
			end
    end

    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE: begin
	// 64 processing rounds steps for 512-bit block 
        if (i <= 64) begin
			if (i < 48) begin
				w[i+16] <= w[i] + (rightrotate(w[i+1],7) ^ rightrotate(w[i+1],18) ^ (w[i+1] >> 3)) + w[i+9] + (rightrotate(w[i+14],17) ^ rightrotate(w[i+14],19) ^ (w[i+14] >> 10));
			end
			a <= sha256_op(a,b,c,d,e,f,g,h,wt,i);
			b <= sha256_op(a,b,c,d,e,f,g,h,wt,i) >> 32;
			c <= sha256_op(a,b,c,d,e,f,g,h,wt,i) >> 64;
			d <= sha256_op(a,b,c,d,e,f,g,h,wt,i) >> 96;
			e <= sha256_op(a,b,c,d,e,f,g,h,wt,i) >> 128;
			f <= sha256_op(a,b,c,d,e,f,g,h,wt,i) >> 160;
			g <= sha256_op(a,b,c,d,e,f,g,h,wt,i) >> 192;
			h <= sha256_op(a,b,c,d,e,f,g,h,wt,i) >> 224;








			i <= i + 1;
			state <= COMPUTE;
        end else begin
			curr_block <= curr_block + 1;
         state <= BLOCK;
		  end
    end

    // h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
    // h0 to h7 after compute stage has final computed hash value
    // write back these h0 to h7 to memory starting from output_addr
    WRITE: begin
			//cur_addr <= output_addr;
			cur_we <= 1;
         case(offset)
             0: begin
                 cur_write_data <= h0;
             end
             1: begin
                 cur_write_data <= h1;
             end
             2: begin
                 cur_write_data <= h2;
             end
             3: begin
                 cur_write_data <= h3;
             end
             4: begin
                 cur_write_data <= h4;
             end
             5: begin
                 cur_write_data <= h5;
             end
             6: begin
                 cur_write_data <= h6;
             end
             default: begin
                 cur_write_data <= h7;
             end
         endcase
			if (offset < 7) begin
				state <= WRITE;
				offset <= offset + 1;
			end
			else begin
				state <= IDLE;
			end
    end
	 endcase
  end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule