`v   .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb
  .global  Main

Main:

  @
  @ Write a program to count the number of unique values in an
  @   array of values stored in memory (ROM)
  @
  @ R1 contains the address of the start of the values in memory
  @   and the first value (at the address in R1) is the count of
  @   the number of values in the array
  @

  MOV   R0, #0          @ result = FALSE;
  LDR   R4, [R1]        @ count = word[adr];
  ADD   R1, R1, #4      @ adr += 4;

  MOV   R5, R1          @ adrOuter = adr;
  MOV   R6, #0          @ countOuter = 0;
whOuter:
  CMP   R6, R4          @ while (countOuter < count)
  BHS   endWhOuter      @ {

  LDR   R7, [R5]        @   valueOuter = word[adrOuter];
  ADD   R5, R5, #4      @   adrOuter += 4;

  MOV   R8, R1          @   adrInner = adr;
  MOV   R12, #1         @   uniqueSoFar = TRUE;
  MOV   R9, #0          @   countInner = 0;
whInner:
  CMP   R9, R6          @   while (countInner < countOuter
  BHS   endWhInner      @          &&
  CMP   R12, #1         @          uniqueSoFar)
  BNE   endWhInner      @   {

  LDR   R10, [R8]       @     valueInner = word[adrInner];
  ADD   R8, R8, #4      @     adrInner += 4;

  CMP   R7, R10         @     if (valueOuter == valueInner)
  BNE   endIfEqual      @     {
  MOV   R12, #0         @       uniqueSoFar = FALSE;
endIfEqual:             @     }

  ADD   R9, R9, #1      @     countInner++;
  B     whInner         @   }
endWhInner:             @

  CMP   R12, #1         @   if (uniqueSoFar)
  BNE   endIfUnique     @   {
  ADD   R0, R0, #1      @     result++;
endIfUnique:            @   }

  ADD   R6, R6, #1      @   countOuter++;
  B     whOuter         @ }
endWhOuter:



  @ End of program ... check your result

End_Main:
  BX    lr

