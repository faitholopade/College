  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

WhileH: 

CMP R0, #13 @ while (h >= 13) {
BLO EndHere 
SUB R0, R0, #12 @ 	h = h - 12;
B WhileH

 EndHere: @ }


  @ End of program ... check your result

End_Main:
  BX    lr

.end