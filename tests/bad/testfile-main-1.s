	.text
	.globl	main
main:
	call F_main
	xorq %rax, %rax
	ret
F_foo:

print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
	.data
S_int:
	.string "%ld"
