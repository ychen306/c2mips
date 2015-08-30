.data
___str32:	.asciiz	"\n"
.text
_sum_arr:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t4,	$a0
	move	$t3,	$a1
	li	$t1,	0
	li	$t2,	0
branch_5:
	slt	$t0,	$t2,	$t3
	beq	$t0,	$zero,	branch_7
	mul	$t0,	$t2,	4
	add	$t0,	$t0,	$t4
	lw	$t0,	0($t0)
	add	$t0,	$t1,	$t0
	move	$t1,	$t0
branch_6:
	move	$t0,	$t2
	add	$t0,	$t0,	1
	move	$t2,	$t0
	j	branch_5
branch_7:
	move	$v0,	$t1
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
main:
	move	$fp,	$sp
	add	$sp,	$sp,	-16
	sw	$ra,	12($sp)
	add	$t1,	$sp,	0
	move	$t1,	$t1
	li	$t0,	0
	add	$t2,	$t0,	$t1
	li	$t0,	1
	sw	$t0,	0($t2)
	li	$t0,	4
	add	$t2,	$t0,	$t1
	li	$t0,	2
	sw	$t0,	0($t2)
	li	$t0,	8
	add	$t0,	$t0,	$t1
	li	$t2,	3
	sw	$t2,	0($t0)
	add	$sp,	$sp,	-12
	move	$a0,	$t1
	li	$a1,	3
	jal	_sum_arr
	move	$t0,	$v0
	add	$sp,	$sp,	12
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_int
	add	$sp,	$sp,	4
	la	$t0,	___str32
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_str
	add	$sp,	$sp,	4
	lw	$ra,	12($sp)
	add	$sp,	$sp,	16
	jr	$ra
_print_str:
	li	$v0,	4
	syscall
	jr	$ra 
_print_int: 
	li	$v0,	1
	syscall
	jr	$ra
