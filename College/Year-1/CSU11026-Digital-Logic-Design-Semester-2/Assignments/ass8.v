 	
     	module FiniteStateMachine(output out_z, input in_x, in_y, clk, reset_b);
     	  reg [1: 0] state, next_state;
     	  reg output_state;
     	  parameter S0 = 2'b00, S1 = 2'b01, S2 = 2'b10, S3 = 2'b11;
     	
     	  // Change state on the rising edge of the clock or rest on negative edge
     	  always @ (posedge clk, negedge reset_b)
     	    begin
    	      if (reset_b == 0)
    	        state <= S0;
    	      else
    	        state <= next_state;
    	    end
    	
    	  always @ (state)
    	    begin
    	      case (state)
    	        S0:
    	          output_state <= 1'b0;
    	        S1:
    	          output_state <= 1'b0;
    	        S2:
    	          output_state <= 1'b1;
    	        S3:
    	          output_state <= 1'b1;
    	      endcase
    	    end
    	
    	
    	  assign out_z = output_state;
    	
    	  always @ (negedge clk)
    	    begin
    	      case (state)
    	        S0:
    	          if({in_x, in_y} == 2'b00)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b01)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b11)
    	            next_state = S1;
    	          else if({in_x, in_y} == 2'b10)
    	            next_state = S3;
    	        S1:
    	          if({in_x, in_y} == 2'b00)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b01)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b11)
    	            next_state = S2;
    	          else if({in_x, in_y} == 2'b10)
    	            next_state = S2;
    	        S2:
    	          if({in_x, in_y} == 2'b00)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b01)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b11)
    	            next_state = S3;
    	          else if({in_x, in_y} == 2'b10)
    	            next_state = S3;
    	        S3:
    	          if({in_x, in_y} == 2'b00)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b01)
    	            next_state = S0;
    	          else if({in_x, in_y} == 2'b11)
    	            next_state = S3;
    	          else if({in_x, in_y} == 2'b10)
    	            next_state = S3;
    	      endcase
    	    end
    	endmodule
