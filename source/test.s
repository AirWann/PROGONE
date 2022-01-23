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
#variable x id 1 position 1
	pushq $0
#variable y id 2 position 2
	pushq $0
#début print
#on regarde la variable x position 1
	movq -8(%rbp), %rdi
	call print_int
	call print_space
#fin print
#début print
#on regarde la variable y position 2
	movq -16(%rbp), %rdi
	call print_int
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
