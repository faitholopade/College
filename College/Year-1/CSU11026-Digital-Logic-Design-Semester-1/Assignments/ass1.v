   	module Circuit_1 (A, B, C, F1, F2);
     	output F1, F2;
     	input A, B, C;
     	wire T1, T2, T3, T4, T5, T6, T7;
     	and (T2, A, B, C);
     	or (T1, A, B, C);
     	and (T4, A, B);
     	and (T5, A, C);
     	and (T6, B, C);
    	or (F2, T4, T5, T6);
    	not (T7, F2);
    	and (T3, T1, T7);
    	or (F1, T2, T3);
    	endmodule