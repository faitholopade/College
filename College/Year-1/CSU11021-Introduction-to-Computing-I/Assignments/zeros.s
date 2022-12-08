  
.syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Follow the instructions in the handout for Assignment 8

MOV R0, #0  // longestSequenceOfZeros = 0
MOV R2, #32 // totalNumberOfBIts = 32
MOV R3, #0  // numberOfBits = 0
MOV R4, #0 // count = 0

While:     // while(bit<=32) {
  CMP R3, #32
  BGE EndWhile
  MOVS R1, R1, LSL #1 // value = value << 1 (LSL #1)
  BCC IncrementCount  // if(carry == 0) {
  MOV R4, #0               // longestSequenceOfZeros = 0
  B EndIncrementCount
  IncrementCount:
    ADD R4, R4, #1 // longestSequenceOfZeros = longestSequenceOfZeros + 1
    CMP R4, R0
    BLT EndIncrementCount
    MOV R0, R4
    B EndIncrementCount 
  EndIncrementCount:    // }
  ADD R3, R3, #1	// numberOfBits = numberOfBits  + 1
B While

EndWhile: // }

  @ End of program ... check your result

End_Main:
  BX    lr

