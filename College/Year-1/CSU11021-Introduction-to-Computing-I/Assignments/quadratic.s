  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Write an ARM Assembly Language program to evaluate
  @   ax^2 + bx + c for given values of a, b, c and x


  MUL R5, R1,R1 @X SQUARED IN R5
  MUL R6, R5, R2 @AX SQUARED IN R6
  MUL R7, R1, R3 @BX IN R7
  ADD R8, R6, R7 
  ADD R9, R8, R4
  MOV R0,R9

  @ End of program ... check your result

End_Main:
  BX    lr
