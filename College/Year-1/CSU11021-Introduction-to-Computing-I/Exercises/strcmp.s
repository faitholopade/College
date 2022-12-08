  .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb
  .global  Main

Main:

LDRB R3, [R1] // char A = byte[address];
LDRB R4, [R2] // char B = byte[address];
MOV R0, #0 // double result;
MOV R5, #0 // boolean = false;

While: // while (char A != 0 && char B !=0 && boolean =false){
CMP R3, #0
BEQ EndWh
CMP R4, #0
BEQ EndWh
CMP R5, #0
BNE EndWh

  If: // if (char A < char B){
    CMP R3, R4
    BGE Else
    MOV R5, #1 // boolean = true;
    MOV R0, #-1 // result = -1;
    B EndIf
    //} 
  Else:  // else if (char A > char B){
    CMP R3, R4
    BLE EndIf
    MOV R5, #1 // boolean = true;
    MOV R0, #1 // result = 1;
  EndIf:
    //}

  ADD R1, R1, #1 // address = address + 1;
  ADD R2, R2, #1 // address = address + 1;

  LDRB R3, [R1] // char A = byte[address];
  LDRB R4, [R2] // char B = byte[address];

  B While
EndWh://}

IfTwo: //if ( char A !=0 && boolean = false){
  CMP R3, #0
  BEQ EndIfTwo
  CMP R5, #0
  BNE EndIfTwo
  MOV R0, #1 // result = 1;
EndIfTwo: //}

IfThree: // if ( char B !=0 && boolean = false){
  CMP R4, #0
  BEQ EndIfThree
  CMP R5, #0
  BNE EndIfThree
  MOV R0, #-1 // result = -1;
EndIfThree:



























  @
  @ Write a program to compare two strings stored in memory, beginning at the
  @   addresses in R1 and R2. The program should store in R0 ...
  @
  @   0  if the strings are the same
  @ 
  @   -1 if the first non-matching character in the first string precedes the
  @      corresponding character in the second string
  @
  @   +1 if the first non-matching character in the first string succeeds the
  @      corresponding character in the second string
  @




  @ End of program ... check your result

End_Main:
  BX    lr

