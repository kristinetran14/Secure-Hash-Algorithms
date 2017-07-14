module sha256(input logic clk, reset_n, start,             
				input logic [31:0] message_addr, size, output_addr,            
				output logic done, mem_clk, mem_we,            
				output logic [15:0] mem_addr,            
				output logic [31:0] mem_write_data,             
				input logic [31:0] mem_read_data); 

    enum logic [3:0] {RESET = 4'b0000, S1 = 4'b0001, S2 = 4'b0010, S3 =  4'b0011, S4 = 4'b0100, S5 = 4'b0101,
								 S6 = 4'b0111} state;
								 
	 // Initialize hash value for the successive 512-bit chunks
	 logic[31:0] h0, h1, h2, h3, h4,h5,h6,h7;
	 logic[31:0]  a, b, c, d, e, f, g, h, S_1, S_0, ch, maj, t1, t2;
	 logic[31:0] s1, s0;

	 
	 // Break chunk into sixteen  32-bit big-endian words 
	 // Extend the sixteen 32-bit words into eighty 32-bit words
	 logic[31:0] w[0:79];
	 logic[7:0] hash_counter,block_counter;
	 					 
	 // Declare temporary sha256 output variables as an input to the testbench
    logic temp_we;
    logic [15:0] temp_mem_addr;
	 logic [15:0] offset;
    logic [31:0] temp_write_data;   
	 assign mem_clk = clk;
	 assign mem_we = temp_we;
    assign mem_addr = temp_mem_addr + offset;
    assign mem_write_data = temp_write_data;
  
	  // convert from little-endian to big-endian
	 function logic [31:0] changeEndian(input logic [31:0] value);
			changeEndian = {value[7:0], value[15:8], value[23:16], value[31:24]};
	 endfunction
	 
	 // Call module call_num_blooks from programming assignement 3
	 logic[15:0] total_num_blocks;
    function logic [15:0] determine_num_blocks(input logic [31:0] size);
       if ((size << 3) % 512 <= 447)
           determine_num_blocks = ((size << 3)/512) + 1;
       else
           determine_num_blocks = ((size << 3)/512) + 2;
    endfunction
	 assign total_num_blocks = determine_num_blocks(size); 
	 
	 // SHA256 K constants
	parameter int sha256_k[0:63] = '{
   32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
	};

	// SHA256 hash round
	function logic [255:0] hash_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
											 input logic [7:0] t);
	begin
		 S_1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
		 ch = (e & f) ^ ((~e) & g);
		 t1 = h + S_1 + ch + sha256_k[t] + w;
		 S_0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
		 maj = (a & b) ^ (a & c) ^ (b & c);
		 t2 = S_0 + maj;
		 hash_op = {t1 + t2, a, b, c, d + t1, e, f, g};
	end
	endfunction
	 
	 // Declare the rightrotate function that can be called by hash_block
	function logic [31:0] rightrotate(input logic [31:0] x,
												 input logic [7:0] r);
	begin
		 rightrotate = (x >> r) | (x << (32-r));
	end
	endfunction

	 
    always_ff @(posedge clk, negedge reset_n)
    begin
	 
        if (!reset_n) begin
            temp_we <= 1'b0;
            temp_mem_addr <= message_addr;
            temp_write_data <= 32'h0;
            state <= RESET;
        end else case (state)
		  
        RESET:
            if (start)
            begin
                block_counter <= 0;
                temp_we <= 1'b0;
                offset <= 0;
                temp_mem_addr <= message_addr;
                temp_write_data <= 32'h0;
					 h0 = 32'h6a09e667;
					 h1 = 32'hbb67ae85;
					 h2 = 32'h3c6ef372;
					 h3 = 32'ha54ff53a;
					 h4 = 32'h510e527f;
					 h5 = 32'h9b05688c;
					 h6 = 32'h1f83d9ab;
					 h7 = 32'h5be0cd19;
					 state <= S1;
            end	
				
        S1:
            if (block_counter < total_num_blocks) begin
                a <= h0;
                b <= h1;
                c <= h2;
                d <= h3;
                e <= h4;
					 f <= h5;
					 g <= h6;
					 h <= h7; 
					 hash_counter <= 0;
                state <= S2;
            end else begin
					 hash_counter <= 0;
                state <= S6;
            end
				
        S2:
            if (hash_counter < 16) begin
                if((offset << 2) <= size && ((offset + 1) << 2) > size)
                    case (size % 4)
						  // From Piazza: AND'ing with 32'hFFFF0000 will "mask off" the lower two bytes, 
						  // and OR'ing with 32'h00008000 will put "80" in the third byte, 
						  // where "80" in hexidecimal is the same as "10000000" in binary
                    0: w[hash_counter] <= 32'h80000000;
                    1: w[hash_counter] <= changeEndian(mem_read_data) & 32'h FF000000 | 32'h 00800000;
                    2: w[hash_counter] <= changeEndian(mem_read_data) & 32'h FFFF0000 | 32'h 00008000;
                    3: w[hash_counter] <= changeEndian(mem_read_data) & 32'h FFFFFF00 | 32'h 00000080;
						  endcase 
                else if (((offset + 1) << 2) <= size)
                    w[hash_counter] <= changeEndian(mem_read_data);
                else if ((block_counter == total_num_blocks - 1) && hash_counter == 15) 
                    w[hash_counter] <= size << 3; 
                else
                    w[hash_counter] <= 32'h00000000;
                offset = offset+1;
                hash_counter <= hash_counter + 1;
                state <= S3;
            end else begin
                state <= S4;
            end	
		  
        S3:
            state <= S2;
				
				
        S4:
            if (hash_counter < 64) begin
                s0 = rightrotate(w[hash_counter-15], 7) ^ rightrotate(w[hash_counter-15], 18) ^ (w[hash_counter-15] >> 3);
                s1 = rightrotate(w[hash_counter-2], 17) ^ rightrotate(w[hash_counter-2], 19) ^ (w[hash_counter-2] >> 10);
                w[hash_counter] = w[hash_counter-16] + s0 + w[hash_counter-7] + s1;
                hash_counter <= hash_counter + 1;
                state <= S4;
            end else begin
					 hash_counter <= 0;
                state <= S5;
            end
				
        S5:
            if (hash_counter < 64) begin
				    {a, b, c, d, e,f,g,h} <= hash_op(a, b, c, d, e,f,g,h, w[hash_counter], hash_counter);
                hash_counter <= hash_counter + 1;
                state <= S5;
            end else begin
                h0 <= h0 + a;
                h1 <= h1 + b;
                h2 <= h2 + c;
                h3 <= h3 + d;
                h4 <= h4 + e;
					 h5 <= h5 + f;
					 h6 <= h6 + g;
					 h7 <= h7 + h;
                block_counter <= block_counter + 1;
                state <= S1;
            end
		  

        S6:
            begin
                if (hash_counter < 8) begin
                    temp_we <= 1;
                    temp_mem_addr <= output_addr;
                    case (hash_counter)
                      0: temp_write_data <= h0;
                      1: temp_write_data <= h1;
                      2: temp_write_data <= h2;
                      3: temp_write_data <= h3;
                      4: temp_write_data <= h4;
							 5: temp_write_data <= h5;
                      6: temp_write_data <= h6;
                      7: temp_write_data <= h7;
                    endcase
                    offset <= hash_counter;
                    hash_counter <= hash_counter + 1;
                    state <= S6;
                end else
                    done <= 1;
            end
        endcase
    end
endmodule