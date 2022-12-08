  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Follow the instructions in the handout for Assignment 7

MOV R2, 'a'                           // chCharacterBefore = 'a'
MOV R3, #0                            // boolean character = false

WhileCharacter:                       // while(character = false){

  CMP R3, #0
  BNE EndWhileCharacter                
  LDRB R4, [R1]                      // chPresentCharacter = byte[address]
      
  IfUpperCaseCharacter:             // if(chPresentCharacter>='A' && chPresentCharacter<='Z'){
    CMP R4, 'A'
    BLT EndIfUpperCaseCharacter
    CMP R4, 'Z'
    BGT EndIfUpperCaseCharacter
    MOV R3, #1                      // boolean character = true
    EndIfUpperCaseCharacter:        //}

  IfLowerCaseCharacter:             // if(chPresentCharacter>='a' && chPresentCharacter<='z'){
    CMP R4, 'a'
    BLT EndIfLowerCaseCharacter
    CMP R4, 'z'
    BGT EndIfLowerCaseCharacter
    MOV R3, #1                      // boolean character = true
    EndIfLowerCaseCharacter:        //}

  IfNoCharacter:                    // if(character = false){
    CMP R3, #1    
    BEQ EndIfNoCharacter    
    ADD R1, R1, #1                  // address = address + 1
    EndIfNoCharacter:               //}

  B WhileCharacter    
EndWhileCharacter:                 //}

  MOV R5, #0                      // boolean letter = false

WhileUpperOrLower:                // while(chPresentCharacter!=NULL){

  CMP R4, #0
  BEQ EndWhileUpperOrLower
  MOV R6, #0                     // boolean DontAddSpace = false

  IfUpperCaseLetterOne:          // if(chPresentCharacter>='A' && chPresentCharacter<='Z'){
    CMP R4, 'A'
    BLT EndIfUpperCaseLetterOne
    CMP R4, 'Z'
    BGT EndIfUpperCaseLetterOne

    IfUpperCaseLetterTwo:        // if(chCharacterBefore >='A' && chCharacterBefore <='Z'){
      CMP R2, 'A'
      BLT EndIfUpperCaseLetterTwo
      CMP R2, 'Z'
      BGT EndIfUpperCaseLetterTwo
      MOV R5, #1                    // boolean letter = true
    EndIfUpperCaseLetterTwo:        //}

    IfLowerCaseLetterOne:           // if(chCharacterBefore >='a' && chCharacterBefore <='z'){
      CMP R2, 'a'
      BLT EndIfLowerCaseLetterOne
      CMP R2, 'z'
      BGT EndIfLowerCaseLetterOne
      MOV R5, #1                  // boolean letter = true
    EndIfLowerCaseLetterOne:      //}

    IfLetter:                     // if(letter = true){
      CMP R5, #1      
      BNE EndIfLetter     
      ADD R4, R4, #32              // chPresentCharacter = chPresentCharacter + ''
      STRB R4, [R0]                // byte[address] = chPresentCharacter
      MOV R6, #1                   // DontAddSpace = true
      SUB R4, R4, #32              // chPresentCharacter = chPresentCharacter - ''
      ADD R0, R0, #1              // byte[address] = chPresentCharacter
    EndIfLetter:                   //}

    IfAddLetterOne:             // if(DontAddSpace = false){
       CMP R6, #1
       BEQ EndIfAddLetterOne
       STRB R4, [R0]            // byte[address] = chPresentCharacter
       ADD R0, R0, #1           // address = address + 1
    EndIfAddLetterOne:          // }

  EndIfUpperCaseLetterOne:    //}

  IfLowerCaseLetterTwo:       // if(chPresentCharacter>='a'&& chPresentCharacter<='z'){

    CMP R4, 'a'
    BLT EndIfLowerCaseLetterTwo
    CMP R4, 'z'
    BGT EndIfLowerCaseLetterTwo

    IfUpperCaseLetterThree:             // if(chCharacterBefore>='A'&& chCharacterBefre<='Z'){
      CMP R2, 'A'
      BLT EndIfUpperCaseLetterThree
      CMP R2, 'Z'
      BGT EndIfUpperCaseLetterThree
      MOV R5, #1                        // boolean letter = true
    EndIfUpperCaseLetterThree:

    IfLowerCaseLetterThree:           // if(chCharacterBefore>='a'&& chCharacterBefore<='z'){
      CMP R2, 'a'
      BLT EndIfLowerCaseLetterThree
      CMP R2, 'z'
      BGT EndIfLowerCaseLetterThree
      MOV R5, #1                       // boolean letter = true
      EndIfLowerCaseLetterThree:       //}

    IfNoLetter:                       // if(letter = false){
      CMP R5, #0
      BNE EndIfNoLetter
      SUB R4, R4, #32                 // chPresentCharacter = chPresentCharacter - ''
      STRB R4, [R0]                   // byte[address] = chPresentCharacter
      ADD R4, R4, #32                 // chPresentCharacter = chPresentCharacter + ''
      MOV R6, #1                      // dontAddSpace = true
      ADD R0, R0, #1                   // address = address + 1
    EndIfNoLetter:                     //}

    IfAddLetterTwo:                   // if(dontAddSpace = false){
       CMP R6, #1
       BEQ EndIfAddLetterTwo
       STRB R4, [R0]                  // byte[address] = chPresentCharacter
       ADD R0, R0, #1                 // address = address + 1
    EndIfAddLetterTwo:                //}
  EndIfLowerCaseLetterTwo:
    

  ADD R1, R1, #1         // address = address + 1
  MOV R2, R4             // chCharacterBefore = chPresentCharacter
  LDRB R4, [R1]           // chPresentCharacter = byte[address]
  MOV R5, #0              // letter = false

  B WhileUpperOrLower
EndWhileUpperOrLower:       //}


MOV R4, #0             // chPresentCharacter = NULL
STRB R4, [R0]           // byte[address] = chPresentCharacter




    























  @
  @ TIP: To view memory when debugging your program you can ...
  @
  @   Add the watch expression to see individual characters: (char[64])newString
  @
  @   OR
  @
  @   Add the watch expression to see the string: (char*)&newString
  @
  @   OR
  @
  @   Open a 'Memory View' specifying the address &newString and length at
  @   least 128. You can open a Memory View with ctrl-shift-p type
  @   'View Memory' (cmd-shift-p on a Mac).
  @

  @ *** your program goes here ***

  @ End of program ... check your result


End_Main:
  BX    lr
