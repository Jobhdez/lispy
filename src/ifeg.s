	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $0, -8(%rbp)
	cmpq $3, -8(%rbp)
	setl %al
	movzbq %al, %rsi
	cmpq $1, %rsi
	je block_0
	jmp block_1
block_0:
	movq $3, %rdi
	callq print_int
	jmp conclusion
block_1:
	movq $4, %rdi
	callq print_int
	jmp conclusion

conclusion:
	addq $16, %rsp
	popq %rbp
	retq