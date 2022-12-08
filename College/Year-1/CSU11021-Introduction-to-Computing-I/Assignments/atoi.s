  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Follow the instructions in the handout for Assignment 2

CMP R1, '@' @if (r1>@), r1=r1-@, r1= r1+9 
BLT ElseInteger @if less than @ i.e if A, B, C, D, E or F
SUB R1, R1, '@' @r1-@ gives value sine r1>@ meaning it was a digit
ADD R1, R1, #9 @to get value A,B,C,D,E OR F (10,11,12,13,14,15) nine needs to be added to value given
B EndWhenInteger
ElseInteger:
SUB R1, R1, '0' @to convert 
EndWhenInteger:

CMP R2, '@'
BLT ElseInteger1
SUB R2, R2, '@'
ADD R2, R2, #9
B EndWhenInteger1
ElseInteger1:
SUB R2, R2, '0'
EndWhenInteger1:

CMP R3, '@'
BLT ElseInteger2
SUB R3, R3, '@'
ADD R3, R3, #9
B EndWhenInteger2
ElseInteger2:
SUB R3, R3, '0'
EndWhenInteger2:

CMP R4, '@'
BLT ElseInteger3
SUB R4, R4, '@'
ADD R4, R4, #9
B EndWhenInteger3
ElseInteger3:
SUB R4, R4, '0'
EndWhenInteger3:
 
MOV R6, #1

MUL R1, R1, R6 
MUL R6, R6, R5 @INCREASING POWER
MUL R2, R2, R6 @MULTIPLYING BY NEW POWER EACH TIME
MUL R6, R6, R5
MUL R3, R3, R6
MUL R6, R6, R5
MUL R4, R4, R6
MUL R6, R6, R5

MOV R0, #0
ADD R0, R1
ADD R0, R2
ADD R0, R3
ADD R0, R4

  @ End of program ... check your result

End_Main:
  BX    lr
