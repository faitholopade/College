  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

MOV R0, #0 // int count = 0;
MOV R2, #2 //int i =2;
CMP R1, #2 // if ( N < 2); {
BLT EndLessThanTwo
// }

 While: // (i < N)  {

 CMP R2, R1 // (i < N);
 BEQ EndWh
 MOV R3, #2 // divisor =2;
 MOV R4, #1 // remainder = 1;

  WhilePrimeOrNot:  //  (divisor < i && remainder !=0) {

    CMP R3, R2 //   (divisor < i);
    BEQ EndWhPrimeOrNot
    CMP R4, #0 // (remainder !=0);
    BEQ EndWhPrimeOrNot

    UDIV R6, R2, R3 // quotient = i / divisor;
    MUL R7, R3, R6 // (quotient * divisor);
    SUB R4, R2, R7 // i -  (quotient * divisor);
    ADD R3, R3, #1 // divisor = divisor +1;

    B WhilePrimeOrNot
// }

EndWhPrimeOrNot:

CMP R4, #0 // (remainder !=0);
BEQ End
ADD R0, R0, #1 // int count = count +1;

End:

ADD R2, R2, #1 // int i = int i + 1;
B While
// }

EndWh:

EndLessThanTwo:

End_Main:
  BX    lr
