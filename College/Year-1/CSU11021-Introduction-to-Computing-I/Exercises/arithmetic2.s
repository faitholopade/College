  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Write a program to compute x^3−4x^2+3x+8 where x is a value in R1
  MUL R0, R1, R1 @X SQUARED IN R0
  MUL R2, R0,R1 @X CUBED IN R2
  MOV R3, #4 @ NUMBER 4 IN R3
  MUL R4, R1, R1 @ X SQUARED IN R4
  MUL R5, R3, R4 @4X SQUARED IN R5
  MOV R3, #3 @ NUMBER 3 IN R3
  MUL R4, R3, R1 @ 3X IN R4
  MOV R6, #8 @NUMBER 8 IN R6
  SUB R7, R2, R5 @ X CUBED MINUS 4X SQUARED IN R7
  ADD R8, R7, R4 @ EQN PLUS 3X IN R8
  ADD R9, R8,R6
  MOV R0, R9
  





  


  @ End of program ... check your result

End_Main:
  BX    lr
