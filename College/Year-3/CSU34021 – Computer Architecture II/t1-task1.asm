include \masm32\include\masm32rt.inc

.data
N DD 6

.code
;;implementation of my_sum3 using _cdecl calling convention
my_sum3 PROC
    ;;function prolog
    push ebp             ;preserving ebp
    mov ebp, esp         ;creating stack frame
    sub esp, 4           ;Space for local variable x

    ;;first if loop code
    mov edx, [ebp+8]     ;getting variable a into a register
    cmp edx, 0           ;comparing a with 0
    jle end_recursion    ;if less equal to zero, I jump to conclude the recursion
    mov [ebp-4], edx.    ;otherwise x = a
    dec edx              ;and a--

    ;;second if loop code
    mov ecx, [ebp+12]    ;getting variable b into a register
    cmp ecx, 0           ;comparing b with 0
    jl next_if           ;if it is less then I jump to the next if check
    mov [ebp-4], ecx     ;otherwise x = x + b
    dec ecx              ;and b--

    ;;third if loop code
    next_if:
    mov edi, [ebp+16]    ;getting variable c into a register
    cmp edi, 0           ;comparing c with 0
    jl recursive_call    ;if it is less then I jump to the recursive call
    mov [ebp-4], edi     ;otherwise x = x + c
    dec edi              ;and c--

    ;;handling the recursive call here
    recursive_call:
    push edi             ;pushing c
    push ecx             ;pushing b
    push edx             ;pushing a
    call my_sum3         ;recursive function call
    add esp, 12          ;cleaning the stack
    add eax, [ebp-4]     ;updating sum result (in eax)
    jmp exit_fun         ;prepare to exit the procedure

    ;;handling the case a<=0
    end_recursion:
    xor eax, eax         ;simply set the result (in eax) to zero

    ;;function epilogue
    exit_fun:
    add esp, 4           ;removing the local variable
    pop ebp              ;restoring ebp
    ret 0                ;returning
my_sum3 ENDP

;;implementation of my_sum3_wrapper using _cdecl calling convention
my_sum3_wrapper PROC
    ;;function prologue
    push ebp             ;preserving ebp
    mov ebp, esp         ;building the stack frame

    ;;retrieving input parameter
    mov edx, [ebp+8]     ;storing N into a register for quick access
    sub edx, 2           ;N = N-2

    ;;handling function call
    push edx             ;pushing N-2 (i.e. last parameter for function call)
    inc edx
    push edx             ;pushing N-1
    inc edx
    push edx             ;pushing N
    call my_sum3         ;calling my_sum3
    add esp, 12          ;cleaning the stack

    ;;function epilogue
    pop ebp              ;restoring ebp
    ret 0                ;returning from procedure
my_sum3_wrapper ENDP

;;implementation of main function
main:
    mov edx, N           ;storing N into a register
    push edx             ;pushing only input of my_sum3_wrapper
    call my_sum3_wrapper ;function calling
    add esp, 4           ;cleaning the stack
    print str$(eax)      ;printing result
    ret 0                ;returning from main
end main
