.data
___str16:	.asciiz	"\n"
.text
fib:
	sub	$sp,	$sp,	12
	sw	$s0,	0($sp)
	sw	$s1,	4($sp)
	sw	$ra,	8($sp)
	move	$s1,	$a0
	slt	$t0,	$s1,	2
	beq	$t0,	$zero,	branch_3
	move	$v0,	$s1
	lw	$s0,	0($sp)
	lw	$s1,	4($sp)
	lw	$ra,	8($sp)
	add	$sp,	$sp,	12
	jr	$ra
	j	branch_4
branch_3:
	sub	$t0,	$s1,	1
	sub	$sp,	$sp,	8
	sub	$fp,	$fp,	20
	move	$a0,	$t0
	jal	fib
	move	$s0,	$v0
	add	$sp,	$sp,	8
	add	$fp,	$fp,	20
	sub	$t0,	$s1,	2
	sub	$sp,	$sp,	8
	sub	$fp,	$fp,	20
	move	$a0,	$t0
	jal	fib
	move	$t0,	$v0
	add	$sp,	$sp,	8
	add	$fp,	$fp,	20
	add	$t0,	$s0,	$t0
	move	$v0,	$t0
	lw	$s0,	0($sp)
	lw	$s1,	4($sp)
	lw	$ra,	8($sp)
	add	$sp,	$sp,	12
	jr	$ra
branch_4:
	lw	$s0,	0($sp)
	lw	$s1,	4($sp)
	lw	$ra,	8($sp)
	add	$sp,	$sp,	12
	jr	$ra
main:
	sub	$sp,	$sp,	8
	sw	$s0,	0($sp)
	sw	$ra,	4($sp)
	li	$s0,	0
branch_11:
	slt	$t0,	$s0,	20
	beq	$t0,	$zero,	branch_13
	sub	$sp,	$sp,	8
	sub	$fp,	$fp,	16
	move	$a0,	$s0
	jal	fib
	move	$t0,	$v0
	add	$sp,	$sp,	8
	add	$fp,	$fp,	16
	sub	$sp,	$sp,	4
	sub	$fp,	$fp,	12
	move	$a0,	$t0
	jal	print_int
	add	$sp,	$sp,	4
	add	$fp,	$fp,	12
	la	$t0,	___str16
	sub	$sp,	$sp,	4
	sub	$fp,	$fp,	12
	move	$a0,	$t0
	jal	print_str
	add	$sp,	$sp,	4
	add	$fp,	$fp,	12
branch_12:
	move	$s0,	$s0
	add	$s0,	$s0,	1
	move	$s0,	$s0
	j	branch_11
branch_13:
	li	$v0,	0
	lw	$s0,	0($sp)
	lw	$ra,	4($sp)
	add	$sp,	$sp,	8
	jr	$ra
print_str:
	li	$v0,	4
	syscall
	jr	$ra 
print_int: 
	li	$v0,	1
	syscall
	jr	$ra
