	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_main:
	pushq %rbp
	movq %rsp, %rbp
#début bloc
#début bloc
#début print
	movq $S_1, %rdi
	call print_string
	call print_space
#fin print
#fin bloc
#fin bloc
	movq %rbp, %rsp
	popq %rbp
	ret

print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_bool:
        test    %rdi, %rdi
        jz      print_false
        mov     $S_true, %rdi
        call    printf
        xorq    %rax, %rax
        ret
print_false:
        mov     $S_false, %rdi
        call    printf
        xorq    %rax, %rax
        ret
print_string:
        test    %rdi, %rdi
        jz      print_nil
        mov     %rdi, %rsi
        mov     $S_string, %rdi
        xorq    %rax, %rax
        call    printf
        ret
print_nil:
        mov     $S_nil, %rdi
        xorq    %rax, %rax
        call    printf
        ret      
print_space:
        mov     $S_space, %rdi
        xorq    %rax, %rax
        call    printf
        ret   
	.data
S_int:
	.string "%ld"
S_true:
	.string "true"
S_false:
	.string "false"
S_string:
	.string "%s"
S_nil:
	.string "<nil>"
S_space:
	.string " "
S_1:
	.string "Hello World !"
