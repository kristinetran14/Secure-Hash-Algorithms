module md5(input logic clk, reset_n, start,             
				input logic [31:0] message_addr, size, output_addr,            
				output logic done, mem_clk, mem_we,            
				output logic [15:0] mem_addr,            
				output logic [31:0] mem_write_data,             
				input logic [31:0] mem_read_data); 

    enum logic [3:0] {RESET = 4'b0000, S1 = 4'b0001, S2 = 4'b0010, S3 =  4'b0011, S4 = 4'b0100, S5 = 4'b0101,
								 S6 = 4'b0111} state;
								 
	 // Initialize hash value for the successive 512-bit chunks
	 logic[31:0] h0, h1, h2, h3;
	 logic[31:0]  a, b, c, d,t1, t2;
	 logic[31:0] w[0:63];

	 
	 // Break chunk into sixteen  32-bit big-endian words 
	 // Extend the sixteen 32-bit words into eighty 32-bit words
	 //logic[31:0] w[0:63]
	 logic[7:0] hash_counter,block_counter;
	 					 
	 // Declare temporary MD5 output variables as an input to the testbench
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
	
	// MD5 K constants
	parameter byte S[0:63] = '{
    8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22, 8'd7, 8'd12, 8'd17, 8'd22,
    8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20, 8'd5, 8'd9,  8'd14, 8'd20,
    8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23, 8'd4, 8'd11, 8'd16, 8'd23,
    8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21, 8'd6, 8'd10, 8'd15, 8'd21
	};

	// MD5 K constants
	parameter int md5_k[0:63] = '{
		 32'hd76aa478, 32'he8c7b756, 32'h242070db, 32'hc1bdceee,
		 32'hf57c0faf, 32'h4787c62a, 32'ha8304613, 32'hfd469501,
		 32'h698098d8, 32'h8b44f7af, 32'hffff5bb1, 32'h895cd7be,
		 32'h6b901122, 32'hfd987193, 32'ha679438e, 32'h49b40821,
		 32'hf61e2562, 32'hc040b340, 32'h265e5a51, 32'he9b6c7aa,
		 32'hd62f105d, 32'h02441453, 32'hd8a1e681, 32'he7d3fbc8,
		 32'h21e1cde6, 32'hc33707d6, 32'hf4d50d87, 32'h455a14ed,
		 32'ha9e3e905, 32'hfcefa3f8, 32'h676f02d9, 32'h8d2a4c8a,
		 32'hfffa3942, 32'h8771f681, 32'h6d9d6122, 32'hfde5380c,
		 32'ha4beea44, 32'h4bdecfa9, 32'hf6bb4b60, 32'hbebfbc70,
		 32'h289b7ec6, 32'heaa127fa, 32'hd4ef3085, 32'h04881d05,
		 32'hd9d4d039, 32'he6db99e5, 32'h1fa27cf8, 32'hc4ac5665,
		 32'hf4292244, 32'h432aff97, 32'hab9423a7, 32'hfc93a039,
		 32'h655b59c3, 32'h8f0ccc92, 32'hffeff47d, 32'h85845dd1,
		 32'h6fa87e4f, 32'hfe2ce6e0, 32'ha3014314, 32'h4e0811a1,
		 32'hf7537e82, 32'hbd3af235, 32'h2ad7d2bb, 32'heb86d391
	};
	
	// MD5 f
	function logic[31:0] md5_f(input logic [7:0] t);
	begin
		 if (t <= 15)
			  md5_f = (b & c) | ((~b) & d);
		 else if (t <= 31)
			  md5_f = (d & b) | ((~d) & c);
		 else if (t <= 47)
			  md5_f = b ^ c ^ d;
		 else
			  md5_f = c ^ (b | (~d));
	end
	endfunction
	
	// MD5 hash op
	function logic[127:0] hash_op(input logic [31:0] a, b, c, d, w,
										  input logic [7:0] t);
	begin
		 t1 = a + md5_f(t) + md5_k[t] + w;
		 t2 = b + ((t1 << S[t])|(t1 >> (32-S[t])));
		 hash_op = {d, t2, b, c};
	end
	endfunction
	
	
	//Calculate_wt 
	function logic[3:0] calculate_w(input logic [7:0] t);
	begin
		if (t <= 15)
			 calculate_w = t;
		else if (t <= 31)
			 calculate_w = 5*t + 1 % 16;
		else if (t <= 47)
			 calculate_w = 3*t + 5 % 16;
		else
			 calculate_w = 7*t % 16;
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
					 h0 <= 32'h67452301;
					 h1 <= 32'hEFCDAB89;
					 h2 <= 32'h98BADCFE;
					 h3 <= 32'h10325476;
                state <= S1;
            end	
				
        S1:
            if (block_counter < total_num_blocks) begin
                a <= h0;
                b <= h1;
                c <= h2;
                d <= h3;
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
					 w[hash_counter] <= w[calculate_w(hash_counter)];
					 hash_counter <= hash_counter + 1;
                state <= S4;
            end else begin
					 hash_counter <= 0;
                state <= S5;
            end
				
        S5:
            if (hash_counter < 64) begin
				    {a, b, c, d} <= hash_op(a, b, c, d, w[hash_counter], hash_counter);
                hash_counter <= hash_counter + 1;
                state <= S5;
            end else begin
                h0 <= h0 + a;
                h1 <= h1 + b;
                h2 <= h2 + c;
                h3 <= h3 + d;
                block_counter <= block_counter + 1;
                state <= S1;
            end
		  
        S6:
            begin
                if (hash_counter < 4) begin
                    temp_we <= 1;
                    temp_mem_addr <= output_addr;
                    case (hash_counter)
                      0: temp_write_data <= h0;
                      1: temp_write_data <= h1;
                      2: temp_write_data <= h2;
                      3: temp_write_data <= h3;
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