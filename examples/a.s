.data
___str12:	.asciiz	"\n"
.text
_print_arr:
	move	$fp,	$sp
	add	$sp,	$sp,	-16
	sw	$s0,	0($sp)
	sw	$s1,	4($sp)
	sw	$s2,	8($sp)
	sw	$ra,	12($sp)
	move	$s2,	$a0
	move	$s1,	$a1
	li	$s0,	0
branch_4:
	slt	$t0,	$s0,	$s1
	beq	$t0,	$zero,	branch_6
	mul	$t0,	$s0,	4
	add	$t0,	$t0,	$s2
	lw	$t0,	0($t0)
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_int
	add	$sp,	$sp,	4
	la	$t0,	___str12
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_str
	add	$sp,	$sp,	4
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
_swap:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t3,	$a0
	move	$t0,	$a1
	move	$t4,	$a2
	mul	$t1,	$t0,	4
	add	$t1,	$t1,	$t3
	lw	$t1,	0($t1)
	move	$t2,	$t1
	mul	$t0,	$t0,	4
	add	$t0,	$t0,	$t3
	mul	$t1,	$t4,	4
	add	$t1,	$t1,	$t3
	lw	$t1,	0($t1)
	sw	$t1,	0($t0)
	mul	$t0,	$t4,	4
	add	$t0,	$t0,	$t3
	sw	$t2,	0($t0)
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
main:
	move	$fp,	$sp
	add	$sp,	$sp,	-20
	sw	$s0,	12($sp)
	sw	$ra,	16($sp)
	add	$s0,	$sp,	0
	move	$s0,	$s0
	li	$t0,	0
	add	$t1,	$t0,	$s0
	li	$t0,	1
	sw	$t0,	0($t1)
	li	$t0,	4
	add	$t0,	$t0,	$s0
	li	$t1,	2
	sw	$t1,	0($t0)
	li	$t0,	8
	add	$t0,	$t0,	$s0
	li	$t1,	3
	sw	$t1,	0($t0)
	add	$sp,	$sp,	-8
	move	$a0,	$s0
	li	$a1,	3
	jal	_print_arr
	add	$sp,	$sp,	8
	add	$sp,	$sp,	-12
	move	$a0,	$s0
	li	$a1,	0
	li	$a2,	2
	jal	_swap
	add	$sp,	$sp,	12
	add	$sp,	$sp,	-8
	move	$a0,	$s0
	li	$a1,	3
	jal	_print_arr
	add	$sp,	$sp,	8
	lw	$s0,	12($sp)
	lw	$ra,	16($sp)
	add	$sp,	$sp,	20
	jr	$ra
_print_str:
	li	$v0,	4
	syscall
	jr	$ra 
_print_int: 
	li	$v0,	1
	syscall
	jr	$ra
