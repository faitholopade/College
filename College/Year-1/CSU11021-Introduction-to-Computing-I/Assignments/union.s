  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  // Follow the instructions in the handout for Assignment 6
LDR   R3, [R1]       // sizeA = word[adrA]
MOV R8, #0          // start = 0
ADD   R1, R1, #4    // adrA += 4
ADD   R7, R0, #4    // tmpAdrC = adrC + 4 // leave space for size
MOV   R4, #0        // sizeC = 0
MOV   R5, #0        // countA = 0

WhCpyA:
  CMP   R5, R3        // while (count < sizeA)
  BHS   EndWhCpyA     // {
  ADD R8, R8, #4     // start = start + 4
  LDR   R6, [R1]      //   elem = word[adrA]
  STR   R6, [R7]      //   word[adrC] = elem
  ADD   R7, R7, #4    //   adrC += 4
  ADD   R4, R4, #1    //   sizeC++
  ADD   R1, R1, #4    //   adrA += 4
  ADD   R5, R5, #1    //   count++
  B     WhCpyA        // }
EndWhCpyA:
SUB R1, R1, #4  // adrA -= 4
//SUB R8, R8, #4  // start = start - 4

MOV R9, #1 // countB =1
LDR R10, [R2] // sizeB = word[adrB]
ADD R2, R2, #4 // adrB += 4

WhCpyB:   // while (countB <= sizeB){
  CMP R9, R10
  BGT EndWhCpyB
  ADD R9, R9, #1
  MOV R11, #0 // boolean = false
  LDR R12, [R2] // elem = word [adrB]
  ADD R2, R2, #4 // adrB += 4
  MOV R5, #1 // countA = 1
  SUB R1, R1, R8 // address = address - start

  WhCpyBTwo: // while (countA <=sizeA){
    CMP R5, R3
    BGT EndWhCpyBTwo
    LDR R6, [R1] // elem = word [adrA]
    ADD R1, R1, #4 // adrA += 4


    If: // if (a==b){
      CMP R6, R12
      BNE EndIf
      MOV R11, #1 // boolean = true
    EndIf: 
    //}

ADD R5, R5, #1 // countA = countA + 1
    B WhCpyBTwo
  EndWhCpyBTwo: 
  //}

    IfTwo: // if (boolean = false){
      CMP R11, #0
      BNE EndIfTwo
      STR R12, [R7] // word[adrC] = B
      ADD R4, R4, #1 // sizeC = sizeC + 1
      ADD R7, R7, #4 // addressC = addressC + 4
    EndIfTwo:
    //}
    B WhCpyB
 EndWhCpyB:
 //}

 STR   R4, [R0]      // word[adrC] = sizeC



































  //
  // TIP: To view the contents of setC while debugging, you can add
  //      the following watch expression, remembering that the first value
  //      listed should be the size of setC and the remaining values shown
  //      should be the values in the set in memory.
  //
  //      (int [64])setC
  //

  // *** your program goes here ***

  // End of program ... check your result


End_Main:
  BX    lr
