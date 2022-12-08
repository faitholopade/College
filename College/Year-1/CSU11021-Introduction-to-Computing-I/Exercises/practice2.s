  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

  CMP R1, #10 @ if (v < 10) {
BHS Else1
MOV R0, #1 @ 	a = 1;
B  EndHere
  Else1: @ }
  CMP R1, #100 @ else if (v < 100) {
  BHS Else2 
  MOV R0, #10 @ 	a = 10;
B  EndHere
  Else2: @ }
  CMP R1, #1000 @ else if (v < 1000) {
BHS Else3 
MOV R0, #100 @ 	a = 100;
B  EndHere
  Else3: @ }
  @ else {
 MOV R0, #0 @ 	a = 0;
  EndHere: @ }  


  

  @ End of program ... check your result

End_Main:
  BX    lr

.end