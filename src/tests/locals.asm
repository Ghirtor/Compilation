.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
	la $t0, arg
	li $t1, 1073741823
	slt $t2, $t1, $v0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $v0, $t1
	bgtz $t2, integer_out_range
	sll $v0, $v0, 1
	sw $v0, 0($t0)
init_end:
	la $t0, arg
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
	subi $t2, $t0, 1
	movn $t0, $t2, $t1
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal main_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	lw $t0, 0($sp)
	la $t1, prog_result
	lw $t2, 0($t1)
	andi $t3, $t2, 1
	sll $t4, $t0, 1
	addi $t5, $t0, 1
	movz $t0, $t4, $t3
	movn $t0, $t5, $t3
	sw $t0, 0($t1)
	li $v0, 10
	syscall
main_integer:
	sw $fp, 0($sp)
	sw $ra, -4($sp)
main_integer_tail_rec_begin:
	move $fp, $sp
	subi $sp, $sp, 24
	addi $sp, $sp, 0
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	jal test
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	li $t0, 79
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal print_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	li $t0, 107
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal print_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	li $t0, 1
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	li $t0, 5
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -4($sp)
	li $t0, 4
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -8($sp)
	li $t0, 3
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -12($sp)
	li $t0, 2
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -16($sp)
	li $t0, 1
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -20($sp)
	li $t0, 6
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -24($sp)
	addi $sp, $sp, -12
	lw $t0, -12($sp)
	lw $t1, -8($sp)
	lw $t2, -4($sp)
	lw $t3, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	move $a1, $t1
	move $a2, $t2
	move $a3, $t3
	jal tailRecFactBisBis_integer_integer_integer_integer_integer_integer_integer
	subi $sp, $sp, -12
	sw $v0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal printInt_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	lw $t0, 0($sp)
	la $t1, h
	lw $t2, 0($t1)
	andi $t3, $t2, 1
	sll $t4, $t0, 1
	addi $t5, $t0, 1
	movz $t0, $t4, $t3
	movn $t0, $t5, $t3
	sw $t0, 0($t1)
	la $t0, h
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
	subi $t2, $t0, 1
	movn $t0, $t2, $t1
	sw $t0, 0($sp)
	li $t0, 720
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	seq $t0, $t0, $t1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	bnez $t0, _label_1
	li $t0, 75
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal print_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	li $t0, 111
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal print_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	b _label_2
_label_1:
	li $t0, 79
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal print_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	li $t0, 107
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal print_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
_label_2:
	li $v0, 0
	move $sp, $fp
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	move $fp, $t1
	move $ra, $t2
	move $a0, $t3
	move $a1, $t4
	move $a2, $t5
	move $a3, $t6
	jr $ra
tailRecFactBisBis_integer_integer_integer_integer_integer_integer_integer:
	sw $fp, 0($sp)
	sw $ra, -4($sp)
tailRecFactBisBis_integer_integer_integer_integer_integer_integer_integer_tail_rec_begin:
	move $fp, $sp
	subi $sp, $sp, 32
	move $t0, $a0
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	sw $t0, -24($fp)
	lw $t0, 12($fp)
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	sw $t0, -28($fp)
	lw $t0, -24($fp)
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	slti $t0, $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	bnez $t0, _label_3
	move $sp, $fp
	addi $sp, $sp, -32
	lw $t0, -24($fp)
	sw $t0, 0($sp)
	lw $t0, -28($fp)
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	mul $t0, $t0, $t1
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	li $t0, 5
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -4($sp)
	li $t0, 4
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -8($sp)
	li $t0, 3
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -12($sp)
	li $t0, 2
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -16($sp)
	li $t0, 1
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -20($sp)
	lw $t0, -24($fp)
	sw $t0, -24($sp)
	lw $t0, -24($sp)
	subi $t0, $t0, 1
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, -24($sp)
	addi $t0, $fp, 12
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	lw $t3, -24($sp)
	move $a0, $t3
	lw $t3, -20($sp)
	move $a1, $t3
	lw $t3, -16($sp)
	move $a2, $t3
	lw $t3, -12($sp)
	move $a3, $t3
	lw $t3, -8($sp)
	sw $t3, -8($t0)
	lw $t3, -4($sp)
	sw $t3, -4($t0)
	lw $t3, 0($sp)
	sw $t3, 0($t0)
	addi $sp, $t0, -12
	sw $t1, 0($sp)
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	jal tailRecFactBisBis_integer_integer_integer_integer_integer_integer_integer_tail_rec_begin
	b _label_4
_label_3:
	lw $t0, -28($fp)
	sw $t0, 0($sp)
	lw $v0, 0($sp)
	move $sp, $fp
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	move $fp, $t1
	move $ra, $t2
	move $a0, $t3
	move $a1, $t4
	move $a2, $t5
	move $a3, $t6
	jr $ra
_label_4:
	li $v0, 0
	move $sp, $fp
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	move $fp, $t1
	move $ra, $t2
	move $a0, $t3
	move $a1, $t4
	move $a2, $t5
	move $a3, $t6
	jr $ra
test:
	sw $fp, 0($sp)
	sw $ra, -4($sp)
test_tail_rec_begin:
	move $fp, $sp
	subi $sp, $sp, 28
	li $t0, 2
	li $t1, 1073741823
	slt $t2, $t1, $t0
	bgtz $t2, integer_out_range
	li $t1, -1073741824
	slt $t2, $t0, $t1
	bgtz $t2, integer_out_range
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	sw $t0, -24($fp)
	lw $t0, -24($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, 0
	lw $t0, 0($sp)
	sw $a0, -8($sp)
	sw $a1, -12($sp)
	sw $a2, -16($sp)
	sw $a3, -20($sp)
	move $a0, $t0
	jal printInt_integer
	subi $sp, $sp, 0
	sw $v0, 0($sp)
	lw $t0, 0($sp)
	sw $t0, -24($fp)
	li $v0, 0
	move $sp, $fp
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	move $fp, $t1
	move $ra, $t2
	move $a0, $t3
	move $a1, $t4
	move $a2, $t5
	move $a3, $t6
	jr $ra
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
	jal print_int_err
	la $a0, invalid_array_access_msg1_part2
	jal print_string
	move $a0, $a2
	jal print_int_err
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
print_int_err:
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
#printInt
printInt_integer:
	sw $fp, 0($sp)
	sw $ra, -4($sp)
printInt_integer_tail_rec_begin:
	move $fp, $sp
	li $v0, 1
	syscall
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	move $fp, $t1
	move $ra, $t2
	move $v0, $a0
	move $a0, $t3
	move $a1, $t4
	move $a2, $t5
	move $a3, $t6
	jr $ra
#print
print_integer:
	sw $fp, 0($sp)
	sw $ra, -4($sp)
print_integer_tail_rec_begin:
	move $fp, $sp
	li $v0, 11
	syscall
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	move $fp, $t1
	move $ra, $t2
	move $v0, $a0
	move $a0, $t3
	move $a1, $t4
	move $a2, $t5
	move $a3, $t6
	jr $ra
#power
power_integer_integer:
	sw $fp, 0($sp)
	sw $ra, -4($sp)
power_integer_integer_tail_rec_begin:
	move $fp, $sp
	move $s0, $a1
	move $s1, $a0
	li $t0, 1
	b power_loop_guard
power_loop_code:
	mul $t0, $t0, $s1
	subi $s0, $s0, 1
power_loop_guard:
	bgtz $s0, power_loop_code
	subi $sp, $sp, 4
	lw $t1, 0($fp)
	lw $t2, -4($fp)
	lw $t3, -8($fp)
	lw $t4, -12($fp)
	lw $t5, -16($fp)
	lw $t6, -20($fp)
	move $v0, $t0
	move $fp, $t1
	move $ra, $t2
	move $a0, $t3
	move $a1, $t4
	move $a2, $t5
	move $a3, $t6
	jr $ra
.data
prog_result:
	.word 0
h:
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
