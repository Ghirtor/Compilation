.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
	la $t0, arg
	sw $v0, 0($t0)
init_end:
	la $t1, i
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	li $t0, 1
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	seq $t0, $t0, $t1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	bnez $t0, _label_1
	li $t0, 46
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	li $t0, 46
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	b _label_2
_label_1:
	li $t0, 46
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	li $t0, 46
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
_label_2:
	li $t0, 0
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, cpt
	sw $t0, 0($t1)
	b _label_6
_label_4:
	li $t0, 0
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	b _label_3
_label_3:
	la $t1, cpt
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	addi $t0, $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, cpt
	sw $t0, 0($t1)
_label_6:
	la $t1, cpt
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	slti $t0, $t0, 2
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	bnez $t0, _label_4
_label_5:
	li $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, c
	sw $t0, 0($t1)
	li $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, acccc
	sw $t0, 0($t1)
	li $t0, 0
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, i
	sw $t0, 0($t1)
	li $t0, 2
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, count
	sw $t0, 0($t1)
	la $t1, i
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	lw $t1, 0($sp)
	addi $t0, $t1, 3
	sw $t0, 0($sp)
	la $t1, i
	lw $t0, 0($t1)
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	add $t0, $t0, $t1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	addi $t0, $t0, 3
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, acc
	sw $t0, 0($t1)
	li $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, accc
	sw $t0, 0($t1)
	b _label_7
_label_8:
	la $t1, arg
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	li $t0, 0
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, c
	sw $t0, 0($t1)
	li $t0, 0
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, j
	sw $t0, 0($t1)
	b _label_10
_label_11:
	la $t1, i
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	la $t1, i
	lw $t0, 0($t1)
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	mul $t0, $t0, $t1
	sw $t0, 0($sp)
	la $t1, j
	lw $t0, 0($t1)
	sw $t0, -4($sp)
	la $t1, j
	lw $t0, 0($t1)
	sw $t0, -8($sp)
	lw $t1, -8($sp)
	lw $t0, -4($sp)
	mul $t0, $t0, $t1
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	add $t0, $t0, $t1
	sw $t0, 0($sp)
	la $t1, arg
	lw $t0, 0($t1)
	sw $t0, -4($sp)
	la $t1, arg
	lw $t0, 0($t1)
	sw $t0, -8($sp)
	lw $t1, -8($sp)
	lw $t0, -4($sp)
	mul $t0, $t0, $t1
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	slt $t0, $t0, $t1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	bnez $t0, _label_13
	li $t0, 35
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	b _label_14
_label_13:
	li $t0, 46
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	li $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, c
	sw $t0, 0($t1)
_label_14:
	li $t0, 32
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	la $t1, j
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	addi $t0, $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, j
	sw $t0, 0($t1)
_label_10:
	la $t1, j
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	la $t1, arg
	lw $t0, 0($t1)
	sw $t0, -4($sp)
	lw $t0, -4($sp)
	addi $t0, $t0, 1
	sw $t0, -4($sp)
	lw $t1, -4($sp)
	lw $t0, 0($sp)
	slt $t0, $t0, $t1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	bnez $t0, _label_11
_label_12:
	li $t0, 10
	sw $t0, 0($sp)
	lw $a0, 0($sp)
	jal print_char
	la $t1, i
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	addi $t0, $t0, 1
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	la $t1, i
	sw $t0, 0($t1)
_label_7:
	la $t1, c
	lw $t0, 0($t1)
	sw $t0, 0($sp)
	lw $t0, 0($sp)
	bnez $t0, _label_8
_label_9:
	li $v0, 10
	syscall
print_char:
	li $v0, 11
	syscall
	jr $ra
sbrk_syscall:
	li $v0, 9
	syscall
	jr $ra
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
cpt:
	.word 0
count:
	.word 0
c:
	.word 0
arg:
	.word 0
acccc:
	.word 0
accc:
	.word 0
acc:
	.word 0
