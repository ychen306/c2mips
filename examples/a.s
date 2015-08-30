.data
___str15:	.asciiz	"\n"
___str18:	.asciiz	"\n"
.text
_swap:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t0,	$a0
	move	$t3,	$a1
	lw	$t1,	0($t0)
	move	$t1,	$t1
	lw	$t2,	0($t3)
	sw	$t2,	0($t0)
	sw	$t1,	0($t3)
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
main:
	move	$fp,	$sp
	add	$sp,	$sp,	-20
	sw	$s0,	8($sp)
	sw	$s1,	12($sp)
	sw	$ra,	16($sp)
	li	$s1,	42
	li	$s0,	1
	sw	$s1,	0($sp)
	add	$s1,	$sp,	0
	sw	$s0,	4($sp)
	add	$s0,	$sp,	4
	add	$sp,	$sp,	-8
	move	$a0,	$s1
	move	$a1,	$s0
	jal	_swap
	add	$sp,	$sp,	8
	lw	$s1,	0($s1)
	add	$sp,	$sp,	-4
	move	$a0,	$s1
	jal	_print_int
	add	$sp,	$sp,	4
	la	$s1,	___str15
	add	$sp,	$sp,	-4
	move	$a0,	$s1
	jal	_print_str
	add	$sp,	$sp,	4
	lw	$s0,	0($s0)
	add	$sp,	$sp,	-4
	move	$a0,	$s0
	jal	_print_int
	add	$sp,	$sp,	4
	la	$s0,	___str18
	add	$sp,	$sp,	-4
	move	$a0,	$s0
	jal	_print_str
	add	$sp,	$sp,	4
	lw	$s0,	8($sp)
	lw	$s1,	12($sp)
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
