  .syntax unified
  .cpu cortex-m4
  .thumb
  .global  Main

Main:

  CMP R1, #'a'@ if (ch=='a' || ch=='e' || ch=='i' || ch=='o' || ch=='u')
  BNE Check2 @ {
  MOV R0, #1
  B EndHere @ 	v = 1;

  Check2: CMP R1, #'a' @ }
  BNE Check3
  MOV R0, #1
  B EndHere 
  
  Check3: CMP R1, #'e'
  BNE Check4
  MOV R0, #1
  B EndHere

  Check4: CMP R1, #'i'
  BNE Check5
  MOV R0, #1
  B EndHere

  Check5: CMP R1, #'o'
  BNE Check6
  MOV R0, #1
  B EndHere

  Check6: CMP R1, #'u'
  BNE Else1
  MOV R0, #1
  B EndHere

  Else1:@ else
  @ {
 MOV R0, #0 @ 	v = 0;
  @ }

EndHere:

End_Main:
  BX    lr

.end