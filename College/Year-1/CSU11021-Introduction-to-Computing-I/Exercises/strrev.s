  .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb
  .global  Main

Main:
  MOV R3, #0

  While: 
    LDRB R2, [R1]         
    CMP R2, #0 
    BEQ EndWh
    ADD R3, R3, #1
    ADD R1, R1, #1 
    B While  
    EndWh:
    SUB R1, R1, #1   
  While2:
    CMP R3, R6
    BEQ EndWh2
    LDRB R2, [R1]
    STRB R2, [R0]
    ADD R0, R0, #1
    SUB R1, R1, #1
    ADD R6, R6, #1
    B While2
  EndWh2:  
   MOV R5, #0
   STRB R5, [R0]
   STRB R5, [R1]
  
   
   
   @
    @ Write a program that will reverse a NULL-terminated string stored in
    @   memory. The original string is stored in memory starting at the
    @   address in R1. Your program should store the reversed string in memory
    @   beginning at the address in R0.
    @


  @
  @ TIP: To view memory when debugging your program you can ...
  @
  @   Add the watch expression to see individual characters: (char[64])strB
  @
  @   OR
  @
  @   Add the watch expression to see the string: (char*)&strB
  @
  @   OR
  @
  @   Open a 'Memory View' specifying the address &strB and length at
  @   least 11. You can open a Memory View with ctrl-shift-p type
  @   'View Memory' (cmd-shift-p on a Mac).
  @

  @ End of program ... check your result

End_Main:
  BX    lr

