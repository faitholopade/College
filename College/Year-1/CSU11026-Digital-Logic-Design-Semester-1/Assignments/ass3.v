
     	primitive UDP_Majority_4 (z, a , b, c, d);
     	    output z;
     	    input a, b, c , d;
     	    table 
     	//        a b c d : z      
     	          0 0 0 0 : 0;
     	          0 0 0 1 : 0;
     	          0 0 1 0 : 0;
     	          0 0 1 1 : 0;
    	          0 1 0 0 : 0;
    	          0 1 0 1 : 0;
    	          0 1 1 0 : 0;
    	          0 1 1 1 : 1;
    	          1 0 0 0 : 0;
    	          1 0 0 1 : 0;
    	          1 0 1 0 : 0;
    	          1 0 1 1 : 1;
    	          1 1 0 0 : 0;
    	          1 1 0 1 : 1;
    	          1 1 1 0 : 1;
    	          1 1 1 1 : 1;
    	    endtable
    	endprimitive