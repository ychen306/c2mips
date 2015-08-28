.data
___str23:	.asciiz	"\n"
.text
_inc:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t1,	$a0
	lb	$t0,	0($t1)
	add	$t0,	$t0,	1
	sb	$t0,	0($t1)
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
_add:
	move	$fp,	$sp
	add	$sp,	$sp,	-28
	sw	$s0,	0($sp)
	sw	$s1,	4($sp)
	sw	$s2,	8($sp)
	sw	$s3,	12($sp)
	sw	$s4,	16($sp)
	sw	$s5,	20($sp)
	sw	$ra,	24($sp)
	move	$s1,	$a0
	move	$s0,	$a1
	move	$s5,	$a2
	move	$s4,	$a3
	add	$s3,	$fp,	8
	add	$s2,	$fp,	9
	add	$sp,	$sp,	-4
	move	$a0,	$s2
	jal	_inc
	add	$sp,	$sp,	4
	add	$s0,	$s1,	$s0
	add	$s0,	$s0,	$s5
	add	$s1,	$s0,	$s4
	lb	$s0,	0($s3)
	add	$s1,	$s1,	$s0
	lb	$s0,	0($s2)
	add	$s0,	$s1,	$s0
	move	$v0,	$s0
	lw	$s0,	0($sp)
	lw	$s1,	4($sp)
	lw	$s2,	8($sp)
	lw	$s3,	12($sp)
	lw	$s4,	16($sp)
	lw	$s5,	20($sp)
	lw	$ra,	24($sp)
	add	$sp,	$sp,	28
	jr	$ra
main:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	add	$sp,	$sp,	-12
	li	$a0,	1
	li	$a1,	2
	li	$a2,	3
	li	$a3,	4
	li	$t0,	5
	sb	$t0,	8($sp)
	li	$t0,	6
	sb	$t0,	9($sp)
	jal	_add
	move	$t0,	$v0
	add	$sp,	$sp,	12
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_int
	add	$sp,	$sp,	4
	la	$t0,	___str23
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_str
	add	$sp,	$sp,	4
	li	$v0,	0
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
_print_str:
	li	$v0,	4
	syscall
	jr	$ra 
_print_int: 
	li	$v0,	1
	syscall
	jr	$ra
