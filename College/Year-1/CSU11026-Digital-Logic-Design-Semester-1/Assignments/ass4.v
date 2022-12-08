	module BCD_Adder (Sum, Carry_out, Addend, Augend, Carry_in);
     	 input [3:0] Addend, Augend;
     		 input Carry_in;
      output [3:0] Sum;
     	 output Carry_out;
     	 reg [4:0] Sum_temp;
     	 reg [3:0] Sum;
     	 reg Carry_out;
    	 always @(Addend, Augend, Carry_in)
    	    begin
    	        Sum_temp = Addend + Augend + Carry_in; //add all the inputs
    	        if(Sum_temp > 9)    begin
    	            Sum_temp = Sum_temp+6; //add 6, if result is more than 9.
    	            Carry_out = 1;  // carry output
    	            Sum = Sum_temp[3:0];    end
    	        else    begin
    	            Carry_out = 0;
    	            Sum = Sum_temp[3:0];
    	        end
    	    end     
    	
    	endmodule
