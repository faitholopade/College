  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

@
@ Follow the instructions contained in the examination paper
@
MOV R0, #0          @ average = 0
LDR R3, [R2]        @ sizeSequence = word[AdrSequence]
ADD R2, R2, #4      @ AdrSequence += 4
MOV R4, #0          @ countOfElements = 0
MOV R6, #0          @ sumElements = 0

WhSum:              @ while(count < sizeSequence) {
  CMP R4, R3        
  BGE EndWhSum
  LDR R5, [R2]      @ element = word[AdrSequence]
  ADD R4, R4, #1    @ countOfElements += 1
  ADD R6, R5, R6    @ sumElements += element
  ADD R2, R2, #4    @ AdrSequence += 4
  B WhSum
EndWhSum:           @ }

MOV R7, #0          @ tempAverage = 0
UDIV R7, R6, R3     @ tempAverage = sumOfElements / sizeSequence
MUL R8, R3, R7      @ quotient = sizeSequence * tempAverage
SUB R9, R6, R8      @ remainder = sumOfElements - quotient

MOV R11, #2         @ divisor = 2
UDIV R10, R3, R11   @ sizeSequence / divisor

IfRemainder:        @ if(remainder > sizeSequence / divisor) {
  CMP R9, R10
  BLE EndIfRemainder
  ADD R7, R7, #1    @ tempAverage += 1
EndIfRemainder:

MOV R0, R7          @ average = tempAverage

MOV R5, #4          @ multiplier = 4
MUL R4, R3, R5      @ resetSequence = sizeSequnce * multiplier
MOV R5, #0          @ resetRegister
SUB R2, R2, R4      @ AdrSequence -= resetSequence  
MOV R5, #1          @ countHighestRepeat = 1 
MOV R7, #1          @ countOfElements = 1
MOV R1, #0          @ mode = 0

WhElements:         @ while(countOfElements < sizeSequence) {
  CMP R7, R3
  BGE EndWhElements
  ADD R7, R7, #1    @ countOfElements += 1
  LDR R8, [R2]      @ element = word[AdrSequence]
  MOV R9, #1        @ repeatsOfElement = 1
  ADD R2, R2, #4    @ AdrSequence += 4

  MOV R10, #1       @ countOfRemainingElements = 1

  WhMode:           @ while(countOfRemainingElements <= sizeSequence) {
    CMP R10, R3
    BGT EndWhMode
    ADD R10,R10, #1 @ countOfRemainingElements += 1
    LDR R12, [R2]   @ temp = word[AdrSequence]
    ADD R9, R9, #1  @ repeatsOfElement +=1

    IfElement:      @ if(element = temp) {
      CMP R8, R12
      BNE EndIfElement
      ADD R9,R9,#1  @ repeatsOfElement +=1
    EndIfElement:   @ }

    B WhMode
  EndWhMode:

  IfCount:          @ if(repeatsOfElement > countHighestRepeat) {
    CMP R9, R5
    BLE EndIfCount
    MOV R5, R9      @ countHighestRepeat = repeatsOfElement
    MOV R1, R8      @ mode = element
  EndIfCount:

  B WhElements
EndWhElements:      @ }








@

End_Main:
  BX    lr
