.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
	la $t0, arg
	sw $v0, 0($t0)
init_end:
	li $t0, -1073741824
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, j
	lw $t2, 0($t1)
	andi $t3, $t2, 1
	sll $t4, $t0, 1
	addi $t5, $t0, 1
	movz $t0, $t4, $t3
	movn $t0, $t5, $t3
	sw $t0, 0($t1)
	la $t0, j
	lw $t0, 0($t0)
	li $t1, 1
	and $t1, $t0, $t1
	move $t2, $t0
	slti $t4, $t2, 0
	srl $t2, $t2, 1
	li $t3, 2147483648
	xor $t5, $t2, $t3
	movn $t2, $t5, $t4
	movz $t0, $t2, $t1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	subi $t0, $t0, 1
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, i
	lw $t2, 0($t1)
	andi $t3, $t2, 1
	sll $t4, $t0, 1
	addi $t5, $t0, 1
	movz $t0, $t4, $t3
	movn $t0, $t5, $t3
	sw $t0, 0($t1)
	li $v0, 10
	syscall
print_char:
	li $v0, 11
	syscall
	jr $ra
sbrk_syscall:
	li $v0, 9
	syscall
	addi $v0, $v0, 4
	sh $t0, -4($v0)
	jr $ra
print_string:
	li $v0, 4
	syscall
	jr $ra
invalid_array_access_1:
	la $a0, invalid_array_access_msg1_part1
	jal print_string
	move $a0, $a1
	jal print_int
	la $a0, invalid_array_access_msg1_part2
	jal print_string
	move $a0, $a2
	jal print_int
	li $v0, 10
	syscall
invalid_array_access_2:
	la $a0, invalid_array_access_msg2
	jal print_string
	li $v0, 10
	syscall
invalid_array_size:
	la $a0, invalid_array_size_msg
	jal print_string
	li $v0, 10
	syscall
print_int:
	li $v0, 1
	syscall
	jr $ra
array_not_initialized:
	la $a0, array_not_initialized_msg
	jal print_string
	li $v0, 10
	syscall
integer_out_range:
	la $a0, integer_out_range_msg
	jal print_string
	li $v0, 10
	syscall
atoi:
	move $t0, $a0
	li $t1, 0
	li $t3, 10
	li $t4, 48
	li $t5, 57
atoi_loop:
	lbu $t2, 0($t0)
	beq $t2, $zero, atoi_end
	blt $t2, $t4, atoi_error
	bgt $t2, $t5, atoi_error
	addi $t2, $t2, -48
	mul $t1, $t1, $t3
	add $t1, $t1, $t2
	addi $t0, $t0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	move $v0, $t1
	jr $ra
.data
j:
	.word 0
i:
	.word 0
arg:
	.word 0
invalid_array_access_msg1_part1:
	.asciiz "\ninvalid array access : size is "
invalid_array_access_msg1_part2:
	.asciiz " but found "
invalid_array_access_msg2:
	.asciiz "\ninvalid array access : negative argument found but positive one expected"
invalid_array_size_msg:
	.asciiz "\ninvalid array size : expected a positive not null argument"
array_not_initialized_msg:
	.asciiz "\narray not initialized"
integer_out_range_msg:
	.asciiz "\ninteger out of range"
