  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Follow the instructions in the handout for Assignment 3
                      @int a, b;
While1:               
  CMP R2, R3          @while (a!=b) {
   BEQ EndHere        @ }
  CMP R2, R3          @if (a>b) {
   BLT Else1
  SUB R2, R2, R3      @a=a-b;}
   B While1
Else1:                @ else{
  SUB R3, R3, R2      @ b=b-a;
   B While1           @ }
EndHere:
  MOV R0, R2



  @ End of program ... check your result

End_Main:
  BX    lr
