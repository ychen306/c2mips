.data
___str4:	.asciiz	"\n"
___str6:	.asciiz	"\n"
.text
main:
	move	$fp,	$sp
	add	$sp,	$sp,	-8
	sw	$s0,	0($sp)
	sw	$ra,	4($sp)
	li	$s0,	42
	move	$t0,	$s0
	add	$s0,	$t0,	1
	move	$s0,	$s0
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_int
	add	$sp,	$sp,	4
	la	$t0,	___str4
	add	$sp,	$sp,	-4
	move	$a0,	$t0
	jal	_print_str
	add	$sp,	$sp,	4
	add	$sp,	$sp,	-4
	move	$a0,	$s0
	jal	_print_int
	add	$sp,	$sp,	4
	la	$s0,	___str6
	add	$sp,	$sp,	-4
	move	$a0,	$s0
	jal	_print_str
	add	$sp,	$sp,	4
	li	$v0,	1
	lw	$s0,	0($sp)
	lw	$ra,	4($sp)
	add	$sp,	$sp,	8
	jr	$ra
_print_str:
	li	$v0,	4
	syscall
	jr	$ra 
_print_int: 
	li	$v0,	1
	syscall
	jr	$ra
