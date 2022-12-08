  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

  @ 
  @ If x is a value in R1, translate the following pseudocode
  @   into ARM Assembly Langauge
  @
  CMP R1, #5 @ if (x >= 5) {
  BLT End1 @   x = 0;
  MOV R1, #0@ }
  MOV R1, #0 @
  End1:

  @ 
  @ If y is a value in R2, translate the following pseudocode
  @   into ARM Assembly Langauge
  @
  CMP R2, #0 @ if (y != 0) {
  BEQ Else1 @   y = y * 2;
  MUL R2, R2, #2
  B EndHere @ }
  Else1: @ else {
  MOV R2, #1 @   y = 1;
   EndHere: 
   @ }
  @

  @ 
  @ If z is a value in R3, translate the following pseudocode
  @   into ARM Assembly Langauge
  @
CMP R3, #13 @ if (z >= 13 && z <= 23) {
BLT ElseIf @   z = z - 12;
CMP R3, #23
BGT ElseIf
SUB R3, R3, #12
B End2
ElseIf:
CMP R3, #0 @ else if (z == 0) {
BNE End2
MOV R3, #12@   z = 12;
End2:
 @ }


  @ 
  @ If a and b are values in R4 and R5, translate the following
  @   pseudocode into ARM Assembly Langauge
  @
 While: 
 CMP R4, #5  @ while (a >= 5) {
 BLT End3
 MUL R6, R4, #2
 ADD R5, R5, R6 @   b = b + (2 * a);
 SUB R4, R4, #5 @   a = a - 5;
 B While @ }
 End3:
  @

End_Main:
  BX    lr

.end