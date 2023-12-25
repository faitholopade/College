.data
buffer db 21 DUP(0)            ;buffer that will hold the string
num dq 1234567890987654321      ;64bit integer to convert to string
N EQU 20                        ;20 is the max num of digits for a 64bit unsigned integer

.code
;;Procedure for integer to string conversion. It follows Microsoft Calling convention
;;The two inputs given are: pointer to the string, and number to translate
my_int2str PROC
    ;;skipping function prologue as I don’t use the stack.
    mov r8, N               ;r8 holds the maximum length = 20
    mov r9, rdx             ;r9 now holds the address of first item of buffer
    add r9, r8              ;r9 now holds second to last address of buffer
    mov rax, rcx            ;rax now holds the number to be translated
    mov r10, 10             ;r10 holds the constant 10

    ;;This loop implements the main logic of the code. The number is repeatedly divided by 10.
    ;;The remainder is stored in the string (in reverse order, and leaving the last char of
    ;;the buffer a terminating character). The quotient (if not null) is instead divided again
    translation_loop:
    dec r9                  ;moving "pointer" back of one position
    xor rdx, rdx            ;xoring rdx before performing division
    div r10                 ;diving rax(=num) by 10. Quotient is rax, remainder is rdx
    add rdx, 48             ;converting digit in rdx to its ASCII code
    mov BYTE PTR [r9], dl   ;storing char in current buffer location
    test rax, rax           ;check if rax is null
    jnz translation_loop    ;if it is not, continue the loop
    mov rax, r9             ;the result is the pointer to the first significant element of the buffer now stored in rax

    ;;function epilogue
    ret 0
my_int2str ENDP

;;This function takes care of printing the buffer. First it computes the length of the buffer
;;by checking for terminating characters, then it uses the standard output interrupt for
;;the printing. It follows Microsoft calling convention.
;;The only input is the pointer to the buffer.
print_buffer PROC
    ;;Skipping prologue because stack is not used
    xor rdx, rdx           ;xoring rdx, it will be the index for the length computation.

    ;;’Length’ computation for buffer.
    ;;It simply iterates updating rdx until the null character is met.
    compute_length:
    inc rdx                ;updating index
    cmp BYTE PTR [rcx+rdx], 0 ;checking if buffer[rdx] is not null...
    jne compute_length     ;...and jumping back to beginning of the loop in that case

    ;;Preparing call to interrupt. rcx and rdx already hold the correct values.
    mov rax, 4
    mov rbx, 1
    int 80h

    ;;function epilogue
    ret 0
print_buffer ENDP

;;Implementation of main function
_start:
    mov rcx, num           ;setting first input to my_int2str
    mov rdx, offset buffer ;setting second input to my_int2str
    sub rsp, 40            ;creating shadow space (and making sure it is aligned...)
    call my_int2str        ;function call to my_int2str
    mov rcx, rax           ;setting first input to print_buffer
    call print_buffer      ;function call to print_buffer
    add rsp, 40            ;cleaning the shadow space

    ;;main epilogue
    mov rax, 1
    mov rbx, 0
    int 80h
end _start
