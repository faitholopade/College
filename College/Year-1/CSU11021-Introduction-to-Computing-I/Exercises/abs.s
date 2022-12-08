  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Write a program to compute the absolute value of an integer in R1

  MOV R0, R1 @result = value
  CMP R0, #0 @ if (result<0)
  BGE EndIfNeg @ if property of result is greater than or equal to zero, remaining operations will not be carried out 
  RSB R0, R0, #0 @ result = 0- result rsb allows us to perform backwards subtraction 
  EndIfNeg: 

  @ End of program ... check your result

End_Main:
  BX    lr
