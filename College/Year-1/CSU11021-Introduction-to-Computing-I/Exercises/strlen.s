  .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb
  .global  Main

Main:

  @ 
  MOV R0, #0 @ Write a program to compute the length of the string
 
  While: @   in memory that begins at the address in R1.
    LDRB R2, [R1]          @first byte loaded in R2
    CMP R2, #0 @ Store your result (the length of the string) in R0.
    BEQ EndWh @
    ADD R0, R0, #1 @ 
    ADD R1, R1, #1 
    B While
  EndWh:
  @ End of program ... check your result

End_Main:
  BX    lr

