	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $65536, %rdi
	movq $65536, %rsi
	callq initialize
	movq rootstack_begin(%rip), %r15
	movq $0, 0(%r15)
	addq $8, %r15
garbage:
	movq free_ptr(%rip), %rax
	addq $32, %rax
	movq fromspace_end(%rip), %r13
	cmpq %r13, %rax
	jl garbage_block_1
	jmp garbage_block_2
garbage_block_1:
	movq $0, %r13
	jmp garbage_block_3
garbage_block_2:
	movq %r15, %rdi
	movq $32, %rsi
	callq collect
	jmp garbage_block_3
garbage_block_3:
	movq free_ptr(%rip), %r11
	addq $32, free_ptr(%rip)
	movq $7, 0(%r11)
	movq $1, 8(%r11)
	movq $2, 16(%r11)
	movq $3, 24(%r11)
	movq 24(%r11), %rdi
	callq print_int
	subq $8, %r15

conclusion:
	addq $0, %rsp
	popq %rbp
	retq