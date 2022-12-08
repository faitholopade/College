  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @
  @ Write a program to multiply R1 by R2 using the
  @   shift-and-add technique from the lectures.
  @   Store the result in R0.

  MOV R0, #0            // result = 0;
  MOV R4, R1            // multiplicand (a)
  MOV R3, R2            // multiplier

  WhileMul:            // while(multiplier!=0) {
    CMP R3, #0
    BEQ EndWhMul
      MOVS R3, R3, LSR #1 // multiplier = multiplier >> 1
      BCC EndIfAdd    // if(carry==1){
      ADD R0, R0, R4 // result = result + multiplicand
      EndIfAdd:       // }
    MOV R4, R4, LSL #1 // multiplicand = multiplicand << 1
    B WhileMul
  EndWhMul:             //}

  @ End of program ... check your result

End_Main:
  BX    lr
