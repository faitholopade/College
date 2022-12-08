  .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb
  .global  Main

Main:

  @
  @ Write a program to compute the sum of 10 word-size values
  @   stored in memory beginning at the address in R1.
  @
  @ Store the result (the sum) in R0
  @

  MOV   R0, #0        @ sum = 0;

  MOV   R2, #0        @ i = 0;
While:
  CMP   R2, #10       @ while (i < 10)
  BHS   EndWhile      @ {
                      @
  LDR   R3, [R1]      @   value = word[address];
  ADD   R0, R0, R3    @   sum = sum + value;
  ADD   R1, R1, #4    @   address = address + 4;
  ADD   R2, R2, #1    @   i = i + 1;
                      @
  B     While         @ }
EndWhile:

  @ End of program ... check your result

End_Main:
  BX    lr

