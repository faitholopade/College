  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

@
@ Follow the instructions contained in the examination paper
@
MOV R0, #0                @ resultOfOperation = 0
MOV R2, #0                @ boolean subtract = false
LDRB R3, [R1]             @ char = byte[AdrExpression]
        
IfSub:                    @ if (char = '-') {
  CMP R3, #'-'       
  BNE EndIfSub        
  MOV R2, #1              @ subtract = true
  ADD R1, R1, #1          @ AdrExpression += 1
  LDRB R3, [R1]           @ char = byte[AdrExpression]
EndIfSub:                 @ }
        
MOV R4, #0                @ currentValue = 0
MOV R5, #10               @ multiplier
        
WhValue:                  @ while(char >= '0' & char <= '9') {
  CMP R3, #'0'        
  BLT EndWhValue        
  CMP R3, #'9'
  BGT EndWhValue
  SUB R3,R3, #'0'         @ char -= '0'
  MUL R4, R4, R5          @ currentValue *= 10
  ADD R4, R4, R3          @ currentValue += char
  ADD R1, R1, #1          @ AdrExpression += 1
  LDRB R3, [R1]           @ char = byte[AdrExpresion]
  B WhValue       
EndWhValue:               @ }
        
MOV R5, #0                @ resetRegister
MOV R5, R4                @ previousValue = currentValue
LDR R6, =-1               @ muliplier = -1
        
IfSubtract:               @ if(subtract = true) {
  CMP R2, #0        
  BNE EndIfSubtract
  MOV R2, #0              @ subtraction = false
  MUL R5, R4, R6          @ currentValue *= -1
EndIfSubtract:            @ }
        
ADD R0, R0, R5            @ resultOfOperation += previousValue
MOV R7, #0                @ functionSelect = 0
        
WhNotNull:                @ while(char != NULL) {
  CMP R3, #'0'        
  BEQ EndWhNotNull        

  IfMultiply:             @ if(char = '*') {
    CMP R3, #'*'        
    BNE Else        
    MOV R7, #1            @ functionSelect = 1
  EndIfMultiply:          @ }
  
  B EndElse

  Else:                   @ else {
    MOV R7, #2            @ functionSelect = 2

    IfSubtraction:        @ if (char = '-') {
      CMP R3, #'-'
      BNE EndIfSubtraction 
      MOV R2, #1  	      @ subtraction = true
    EndIfSubtraction:

  EndElse:

ADD R1, R1, #1            @ AdrExpression += 1
LDRB R3, [R2]             @ char = byte[AdrExpression]

  IfNotSubtract:          @ if(subtraction = false & char = '-') {
    CMP R2, #0
    BNE EndIfNotSubtract
    CMP R3, #'-'
    BNE EndIfNotSubtract
    MOV R2, #1            @ subtraction = true
    ADD R1, R1, #1        @ AdrExpression += 1
    LDRB R3, [R2]         @ char = byte [AdrExpression]
  EndIfNotSubtract:

MOV R4, #0                @ currentValue = 0

  WhChar:                 @ while(char >= '0' & char <= '9') {
    CMP R3, #'0'
    BLT EndWhChar
    CMP R3, #'9'
    BGT EndWhChar
    SUB R3,R3, #'0'       @ char -= '0'
    MUL R4, R4, R5        @ currentValue *= 10
    ADD R4, R4, R3        @ currentValue += char
    ADD R1, R1, #1        @ AdrExpression += 1
    LDRB R3, [R1]         @ char = byte[AdrExpresion]
    B WhChar
  EndWhChar:              @ }

  IfSubtractTwo:          @ if(subtraction = true) {
    CMP R2, #0
    BNE EndIfSubtractTwo
    MOV R2, #0            @ subtraction = false
    MUL R4, R4, R6        @ currentValue *= -1
  EndIfSubtractTwo:       @ }

  IfFunction:             @ if(functionSelect = 1) {
    CMP R7, #1
    BNE ElseFunction
    SUB R0, R0, R5        @ resultOfOperation -= previousValue
    MUL R8, R5, R4        @ mutiplication = previousValue * currentValue
    ADD R0, R0, R8        @ resultOfOperation += multiplication
  EndIfFunction:          @ }

  B EndElseFunction

  ElseFunction:           @ else {
    ADD R0, R0, R4        @ resultOfOperation += currentValue
  EndElseFunction:        @ }

  MOV R5, R4              @ previousValue = currentValue

  B WhNotNull
EndWhNotNull:             @ }

@

End_Main:
  BX    lr
