.data
___str8:	.asciiz	"\n"
.text
main:
	sub	$sp,	$sp,	16
	sw	$s0,	0($sp)
	sw	$s1,	4($sp)
	sw	$s2,	8($sp)
	sw	$ra,	12($sp)
	li	$s2,	0
	li	$s1,	1
	li	$s0,	0
branch_4:
	slt	$t0,	$s0,	10
	beq	$t0,	$zero,	branch_6
	sub	$sp,	$sp,	4
	sub	$fp,	$fp,	20
	move	$a0,	$s2
	jal	print_int
	add	$sp,	$sp,	4
	add	$fp,	$fp,	20
	la	$t0,	___str8
	sub	$sp,	$sp,	4
	sub	$fp,	$fp,	20
	move	$a0,	$t0
	jal	print_str
	add	$sp,	$sp,	4
	add	$fp,	$fp,	20
	add	$t0,	$s2,	$s1
	move	$t0,	$t0
	move	$s2,	$s1
	move	$s1,	$t0
branch_5:
	move	$t0,	$s0
	add	$t0,	$t0,	1
	move	$s0,	$t0
	j	branch_4
branch_6:
	lw	$s0,	0($sp)
	lw	$s1,	4($sp)
	lw	$s2,	8($sp)
	lw	$ra,	12($sp)
	add	$sp,	$sp,	16
	jr	$ra
print_str:
	li	$v0,	4
	syscall
	jr	$ra 
print_int: 
	li	$v0,	1
	syscall
	jr	$ra

