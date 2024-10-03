	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $3, %rax
	addq $4, %rax
	movq %rax, %rdi
	callq print_int

conclusion:
	addq $0, %rsp
	popq %rbp
	retq