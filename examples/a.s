.data
.text
_sum_foo:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t2,	$a0
	add	$t0,	$t2,	0
	lw	$t1,	0($t0)
	add	$t0,	$t2,	4
	lb	$t0,	0($t0)
	add	$t1,	$t1,	$t0
	add	$t0,	$t2,	5
	lb	$t0,	0($t0)
	add	$t1,	$t1,	$t0
	add	$t0,	$t2,	8
	lw	$t0,	0($t0)
	add	$t0,	$t1,	$t0
	move	$v0,	$t0
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
main:
	move	$fp,	$sp
	add	$sp,	$sp,	-20
	sw	$ra,	16($sp)
	add	$t2,	$sp,	0
	add	$t0,	$t2,	4
	add	$t1,	$t0,	0
	li	$t0,	1
	sw	$t0,	0($t1)
	add	$t0,	$t2,	4
	add	$t0,	$t0,	4
	li	$t1,	3
	sb	$t1,	0($t0)
	add	$t0,	$t2,	4
	add	$t1,	$t0,	5
	li	$t0,	5
	sb	$t0,	0($t1)
	add	$t0,	$t2,	4
	add	$t0,	$t0,	8
	li	$t1,	7
	sw	$t1,	0($t0)
	move	$t0,	$t2
	li	$t1,	4
	add	$t0,	$t1,	$t0
	add	$sp,	$sp,	-8
	move	$a0,	$t0
	jal	_sum_foo
	move	$t0,	$v0
	add	$sp,	$sp,	8
	add	$sp,	$sp,	-8
	move	$a0,	$t0
	jal	_print_int
	add	$sp,	$sp,	8
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
