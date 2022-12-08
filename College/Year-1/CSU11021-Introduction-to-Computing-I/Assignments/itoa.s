  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:
  
  @ Follow the instructions in the handout for Assignment 5

CMP R1, #0 // if (int signedValue == 0){
BNE ElseValue
MOV R4, '0' // char 1 = '0';
STRB R4, [R0] // byte [address] = char 1;
ADD R0, R0, #1 // address = address + 1;
MOV R11, #0 // final double NULL = 0;
STRB R11, [R0] // byte [address] = NULL;
B EndIfValue // }

ElseValue:   // else  {
    CMP R1, #0   // if (int signed value > 0){
    BLT ElseNegative
    MOV R2, #'+' // char 2 = '+';
    STRB R2, [R0] // byte [address] = char 2;
    B EndIfPositiveOrNegative
  
  ElseNegative:  // else {
    MOV R2, #'-' // char 3 = '+';
    STRB R2, [R0] // byte [address] = char 3;
    MOV R3, #-1  // final double VALUE_TO_MAKE_POSITIVE = -1;
    MUL R1, R1, R3 // double signedValue *= VALUE_TO_MAKE_POSITIVE;

  EndIfPositiveOrNegative: //}
    MOV R5, R1 // double copyOfSignedValue = signedValue;
    MOV R6, #1 // double powerOfTen = 1;
    MOV R7, #10 // final double VALUE_TO_INCREASE_OR_DECREASE_POWER = 10;
  
      While:  // while (copyOfSignedValue > 9){
        CMP R5, #9 
        BLE EndWh
        UDIV R5, R5, R7 // double copyOfSignedValue = copyOfSignedValue / VALUE_TO_INCREASE_POWER;
        MUL R6, R6, R7 // double powerOfTen = powerOfTen * VALUE_TO_INCREASE_POWER;
        B While
    
      EndWh: //}
      MOV R5, #0 // double count = 0;
      MOV R2, #0 // double firstDigit = 0;
      MOV R4, R1 // double copyOfSignedValue2 = signedValue;
      UDIV R2, R4, R7 // double firstDigit =  copyOfSignedValue2 / final double VALUE_TO_INCREASE_OR_DECREASE_POWER;
      MUL R11, R2, R7 // double firstDigitByTen = lastDigit * final double VALUE_TO_INCREASE_OR_DECREASE_POWER;
      SUB R10, R4, R11 // double lastDigit =  copyOfSignedValue2 - firstDigitByTen;
      UDIV R4, R4, R7 // double copyOfSignedValue2 = copyOfSignedValue2 / final double VALUE_TO_INCREASE_OR_DECREASE_POWER;
      CMP R10, #0 // if (lastDigit ==0){
      BNE EndIfCount1
      ADD R5, R5, #1 // double count = count + 1
      EndIfCount1: //}

      While3:  // while (lastDigit ==0){
        CMP R10, #0
        BNE EndWh3
        UDIV R2, R4, R7 // double firstDigit =  copyOfSignedValue2 / final double VALUE_TO_INCREASE_OR_DECREASE_POWER;
        MUL R11, R2, R7 // double firstDigitByTen = lastDigit * final double VALUE_TO_INCREASE_OR_DECREASE_POWER;
        SUB R10, R4, R11 // double lastDigit =  copyOfSignedValue2 - firstDigitByTen;
        UDIV R4, R4, R7 // double copyOfSignedValue2 = copyOfSignedValue2 / final double VALUE_TO_INCREASE_OR_DECREASE_POWER;
        CMP R10, #0 // // if (lastDigit ==0){
        BNE EndIfCount2
        ADD R5, R5, #1 // // double count = count + 1
        EndIfCount2: //}
        B While3
      EndWh3: //}
      

      MOV R12, #1 // double storeDigits;

      While2:           // while(signedValue !=0){
        CMP R1, #0 
        BEQ EndWh2
        UDIV R8, R1, R6 // double quotient = signedValue / powerOfTen;
        ADD R12, R8, #'0' // char 4 = (char) quotient;
        ADD R0, R0, #1 // address = address + 1;
        STRB R12, [R0] // byte [address] = char 4;
        MUL R9, R6, R8 // double valueForSubtraction = powerOfTen * quotient;
        SUB R1, R1, R9 // double signedValue = signedValue - valueForSubtraction;
        UDIV R6, R6, R7 // double powerOfTen = powerOfTen / VALUE_TO_INCREASE_OR_DECREASE_POWER;
       B While2
  
      EndWh2: // }

      MOV R12, #1 // double storeDigits;

      While4: // while (storeDigits <= count){
        CMP R12, R5 
        BGT EndWh4
        MOV R14, #'0' // char 5 = '0';
        ADD R0, R0, #1 // address = address + 1;
        STRB R14, [R0] // byte [address] = char 5;
        ADD R12, R12, #1 // double storeDigits = storeDigits + 1;
        B While4
      EndWh4: //}

        ADD R0, R0, #1 // address = address + 1
        MOV R10, #0 // final double NULL = 0;
        STRB R10, [R0] // byte [address] = NULL;

EndIfValue: //}


End_Main:
   BX    lr
