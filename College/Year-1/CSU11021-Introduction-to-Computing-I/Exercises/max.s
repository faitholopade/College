  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Write a program to compute the maximum of the values in R1 and R2

 CMP R1, R2 @IF (A>=B)
 BLT ElseMaxB
 MOV R0, R1 @MAX =A
 B EndMax
 ElseMaxB: @ELSE
 MOV R0, R2 @MAX =B
 EndMax:

  @ End of program ... check your result

End_Main:
  BX    lr
