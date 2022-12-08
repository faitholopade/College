module Compare(A, B, Y)
    input [3:0] A;
    input [3:0] B;
    output reg[5:0] Y;

         always @(A or B)
          begin
     
            if(A > B)       begin  
    		    Y[3] = 1;
                end
            else            begin
                Y[3] = 0;
                end
            if(A == B)      begin
    			Y[5] = 1;
    	        end
            else            begin
                Y[5] = 0;
                end
    		if(A < B)	    begin
    			Y[2] = 1;
    			end
            else            begin
                Y[2] = 0;
                end
    		if(A != B)	    begin
    			Y[4] = 1;
    			end
            else            begin
                Y[4] = 0;
                end
    		if(A>= B)       begin
    			Y[1] = 1;
    			end
            else            begin
                Y[1] = 0;
                end
    		if(A<= B)	    begin
    			Y[0] = 1;
    			end
            else            begin
                Y[0] = 0;
    	    end
          end
endmodule