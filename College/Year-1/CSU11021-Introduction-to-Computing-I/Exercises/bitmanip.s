  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @
  @ Write a program to implement all of the following operations:
  @
  
  LDR R4, =0xFF0000FF @   1. Clear (i.e. change to zero) the middle two bytes of the
  AND R1, R1, R4     @      value in R1.
                     
   @
  LDR R5, =0xF0000000 @   2. Set (i.e. change to one) the four most significant bits
  ORR R2, R2, R5     @      of the value in R2.
                    
   @
  LDR R6, =0x55555555 @   3. Invert every second bit of the value in R3, starting
  EOR R3, R3, R6     @      with the least significant bit.
                     @

  @ *** your solution goes here ***

  @ End of program ... check your result

End_Main:
  BX    lr
