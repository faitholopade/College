  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

 CMP R0, #'A' 
 BLO   EndHere @ if (ch >= 'A' && ch <= 'Z') {
 CMP R0, #'Z'
 BHI   EndHere
 ADD R0, R0, #0x20 @ 	ch = ch + 0x20;

 
 EndHere:  @ }

 

  @ End of program ... check your result

End_Main:
  BX    lr

.end