;; Task 1: Translation of function max3

;; Agreed calling convention:
;; Params passed through R26-R31
;; Local variables: R16-R25
;; Params for child function: R10-R15
;; R1 for returning value
;; R31 for returning address (to parent)
;; R15 for returning address for child

min3:
    add R26, R0, R1       ; R1 used for m [output variable]: m=a
min3_1st_if:
    sub R27, R1, R0 {C}   ; set flags for b<m
    jge min3_2nd_if       ; if false, go to second if statement
    xor R0, R0, R0        ; NOP
    add R27, R0, R1       ; if b<m, then m=b
min3_2nd_if:
    sub R28, R1, R0 {C}   ; set flags for c<m
    jge min3_exit         ; if false, go to end of function
    xor R0, R0, R0        ; NOP
    add R28, R0, R1       ; if c<m, then m=c
min3_exit:
    ret (R31)0            ; return to parent function. Output in R1
    xor R0, R0, R0        ; NOP

;; Unoptimized version of min5
min5:
    ;; Preparing function call min3(a,b,c)
    add R26, R0, R10      ; First input of child goes in R10
    add R27, R0, R11      ; Second input goes in R11
    add R28, R0, R12      ; Third input in R12
    callr min3, R15       ; Call min3(a,b,c)
    xor R0, R0, R0        ; NOP
    ;; Preparing function call min3(min3(a,b,c),d,e)
    add R1, R0, R10       ; Output of 1st function call is first input to 2nd call
    add R29, R0, R11      ; Second input is d
    add R30, R0, R12      ; Third input is e
    callr min3, R15       ; Call min3(min3(a,b,c),d,e)
    xor R0, R0, R0        ; NOP
    ret R31(0)            ; Returning to caller
    xor R0, R0, R0        ; NOP

;; Optimized version of min5
min5:
    ;; Preparing function call min3(a,b,c)
    add R26, R0, R10      ; First input of child goes in R10
    add R27, R0, R11      ; Second input goes in R11
    callr min3, R15       ; Call min3(a,b,c)
    add R28, R0, R12      ; Third input in R12 in delay slot
    ;; Preparing function call min3(min3(a,b,c),d,e)
    add R1, R0, R10       ; Output of 1st function call is first input to 2nd call
    add R29, R0, R11      ; Second input is d
    callr min3, R15       ; Call min3(min3(a,b,c),d,e)
    add R30, R0, R12      ; Third input is e (set in delay slot)
    ret R31(0)            ; Returning to caller
    xor R0, R0, R0        ; NOP

;; Optimized main function
main:
    add R0, #100, R10     ; Setting up first param
    add R0, #89, R11      ; Setting up second param
    add R0, #65, R12      ; Setting up third param
    add R0, #12, R13      ; Setting up fourth param
    callr min5, R15       ; Call to min5
    add R0, #32, R14      ; Setting up fifth param in delay slot
    add R1, R0, R16       ; Storing output in local var
    sub R16, R10, R0 {C}  ; Set flags for out-a
    jne 2nd_check
    add R0, #49, R10      ; Loading char "1" in input in delay slot
    call print_char, R15
2nd_check:
    sub R16, R11, R0 {C}  ; Set flags for out-b in delay slot
    jne 3rd_check
    add R0, #50, R10      ; Char "2" in input in delay slot
    call print_char, R15
3rd_check:
    sub R16, R12, R0 {C}  ; Set flags for out-c in delay slot
    jne 4th_check
    add R0, #51, R10      ; Char "3" in input in delay slot
    call print_char, R15
4th_check:
    sub R16, R13, R0 {C}  ; Set flags for out-d in delay slot
    jne 5th_check
    add R0, #52, R10      ; Char "4" in input in delay slot
    call print_char, R15
5th_check:
    sub R16, R14, R0 {C}  ; Set flags for out-e in delay slot
    jne end_check
    add R0, #53, R10      ; Char "5" in input in delay slot
    call print_char, R15
    xor R0, R0, R0        ; NOP
end_check:
    ret (R31)0            ; Returning to caller
    xor R0, R0, R0        ; NOP
