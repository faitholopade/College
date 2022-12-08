  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @
  @ Write a program to swap the middle two bytes of the value in
  @   R4, leaving the outer two bytes unchanged
  @
  @ For example, if R4 initially contains 0x89ABCDEF, your program
  @   should change R4 to 0x89CDABEF
  @
AND R5, R4, #0x00FF0000            // extract AB 0xAB0000
MOV R5, R5, LSR #8            // shift into correct position 0x0000AB00
AND R6, R4, #0x0000FF00            // extract out 0x0000CD00
MOV R6, R6, LSL #8           // shift into correct position 0x00CD0000
LDR  R7, =0x00FFFF00          // orginal value and clear middle to bytes 0x890000EF
BIC R4, R4, R7
ORR R4, R4, R5            // merge back in AB and CD into value of cleared 0x89CDAB00
ORR R4, R4, R6  
  @ End of program ... check your result

End_Main:
  BX    lr
