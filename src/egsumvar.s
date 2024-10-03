	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $3, -8(%rbp)
	movq $4, -16(%rbp)
	movq -8(%rbp), %rax
	addq %rax, -16(%rbp)
	movq -16(%rbp), %rdi
	callq print_int

conclusion:
	addq $16, %rsp
	popq %rbp
	retq