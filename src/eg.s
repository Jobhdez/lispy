	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $3, -8(%rbp)
	addq $4, -8(%rbp)
	movq -8(%rbp), %rdi
	callq print_int

conclusion:
	addq $16, %rsp
	popq %rbp
	retq