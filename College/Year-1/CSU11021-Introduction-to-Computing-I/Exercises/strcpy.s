  .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb
  .global  Main

Main:

  @
  @ Write aprogram that will make a copy of a NULL-terminated string stored in
  While: @   memory. The original string is stored in memory starting at the address
    LDRB R2, [R1] @   in R1. Store the new copy of the string in memory beginning at the address
    CMP R2, #0 @   in R0.
    BEQ EndWh @
    STRB R2, [R0]
    ADD R1, R1, #1
    ADD R0, R0, #1
    B While
  EndWh:
    MOV R3, #0
    STRB R3, [R0]
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

