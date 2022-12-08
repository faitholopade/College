  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

CMP R1, R2  @ if (a < b) @ A R1, B R2 , R R0
 BHS Else1 @ {
MOV R0, R1 @   r = a;
 B EndHere @ }
 Else1:  @ else {
MOV R0, R2  @   r = b;
 EndHere: @ }  

 

  @ End of program ... check your result

End_Main:
  BX    lr

.end