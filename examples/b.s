.data
global_base:	.word	0
___str83:	.asciiz	", "
___str111:	.asciiz	"\ndone! \n"
.text
_find_free_block:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t5,	$a0
	move	$t4,	$a1
	lw	$t1,	global_base
	move	$t3,	$t1
	nop
branch_5:
	beq	$t3,	0,	branch_8
	move	$t0,	$t3
	lw	$t0,	8($t0)
	beq	$t0,	0,	branch_13
	move	$t1,	$t3
	lw	$t1,	0($t1)
	sge	$t1,	$t1,	$t4
	sne	$t2,	$t1,	0
branch_13:
	sne	$t0,	$t0,	0
	and	$t0,	$t0,	$t2
	seq	$t0,	$t0,	$zero
	sne	$t0,	$t0,	0
branch_8:
	sne	$t1,	$t3,	0
	and	$t1,	$t1,	$t0
	beq	$t1,	$zero,	branch_7
	sw	$t3,	0($t5)
	move	$t1,	$t3
	lw	$t1,	4($t1)
	move	$t3,	$t1
branch_6:
	nop
	j	branch_5
branch_7:
	move	$v0,	$t3
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
_request_space:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t4,	$a0
	move	$t1,	$a1
	li	$a0,	0
	li	$v0,	9
	syscall
	move	$t2,	$v0
	add	$t0,	$t1,	12
	move	$a0,	$t0
	li	$v0,	9
	syscall
	move	$t0,	$v0
	sub	$t3,	$zero,	1
	seq	$t0,	$t0,	$t3
	beq	$t0,	$zero,	branch_31
	li	$v0,	0
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
	j	branch_31
branch_31:
	beq	$t4,	$zero,	branch_32
	move	$t0,	$t4
	sw	$t2,	4($t0)
	j	branch_32
branch_32:
	move	$t0,	$t2
	sw	$t1,	0($t0)
	move	$t1,	$t2
	li	$t0,	0
	sw	$t0,	4($t1)
	move	$t1,	$t2
	li	$t0,	0
	sw	$t0,	8($t1)
	move	$v0,	$t2
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
_malloc:
	move	$fp,	$sp
	add	$sp,	$sp,	-16
	sw	$s0,	4($sp)
	sw	$s1,	8($sp)
	sw	$ra,	12($sp)
	move	$s0,	$a0
	sle	$t0,	$s0,	0
	beq	$t0,	$zero,	branch_42
	li	$v0,	0
	lw	$s0,	4($sp)
	lw	$s1,	8($sp)
	lw	$ra,	12($sp)
	add	$sp,	$sp,	16
	jr	$ra
	j	branch_42
branch_42:
	lw	$t0,	global_base
	seq	$t0,	$t0,	$zero
	beq	$t0,	$zero,	branch_45
	li	$a0,	0
	move	$a1,	$s0
	add	$sp,	$sp,	-12
	jal	_request_space
	add	$sp,	$sp,	12
	move	$t0,	$v0
	move	$t1,	$t0
	seq	$t0,	$t1,	$zero
	beq	$t0,	$zero,	branch_49
	li	$v0,	0
	lw	$s0,	4($sp)
	lw	$s1,	8($sp)
	lw	$ra,	12($sp)
	add	$sp,	$sp,	16
	jr	$ra
	j	branch_49
branch_49:
	sw	$t1,	global_base
	j	branch_46
branch_45:
	lw	$t0,	global_base
	sw	$t0,	0($sp)
	add	$s1,	$sp,	0
	move	$a0,	$s1
	move	$a1,	$s0
	add	$sp,	$sp,	-12
	jal	_find_free_block
	add	$sp,	$sp,	12
	move	$t0,	$v0
	move	$t1,	$t0
	seq	$t0,	$t1,	$zero
	beq	$t0,	$zero,	branch_55
	lw	$t0,	0($s1)
	move	$a0,	$t0
	move	$a1,	$s0
	add	$sp,	$sp,	-12
	jal	_request_space
	add	$sp,	$sp,	12
	move	$t0,	$v0
	move	$t1,	$t0
	seq	$t0,	$t1,	$zero
	beq	$t0,	$zero,	branch_60
	li	$v0,	0
	lw	$s0,	4($sp)
	lw	$s1,	8($sp)
	lw	$ra,	12($sp)
	add	$sp,	$sp,	16
	jr	$ra
	j	branch_60
branch_60:
	j	branch_56
branch_55:
	move	$s0,	$t1
	li	$t0,	0
	sw	$t0,	8($s0)
branch_56:
branch_46:
	li	$t0,	12
	add	$t0,	$t0,	$t1
	move	$v0,	$t0
	lw	$s0,	4($sp)
	lw	$s1,	8($sp)
	lw	$ra,	12($sp)
	add	$sp,	$sp,	16
	jr	$ra
_get_block_ptr:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t0,	$a0
	li	$t1,	12
	sub	$t0,	$t1,	$t0
	move	$v0,	$t0
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
_free:
	move	$fp,	$sp
	add	$sp,	$sp,	-4
	sw	$ra,	0($sp)
	move	$t1,	$a0
	seq	$t0,	$t1,	$zero
	beq	$t0,	$zero,	branch_71
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
	j	branch_71
branch_71:
	move	$a0,	$t1
	add	$sp,	$sp,	-8
	jal	_get_block_ptr
	add	$sp,	$sp,	8
	move	$t0,	$v0
	move	$t1,	$t0
	li	$t0,	1
	sw	$t0,	8($t1)
	lw	$ra,	0($sp)
	add	$sp,	$sp,	4
	jr	$ra
_print_list:
	move	$fp,	$sp
	add	$sp,	$sp,	-8
	sw	$s0,	0($sp)
	sw	$ra,	4($sp)
	move	$s0,	$a0
	nop
branch_77:
	sne	$t0,	$s0,	0
	beq	$t0,	$zero,	branch_79
	move	$t0,	$s0
	lw	$t0,	0($t0)
	move	$a0,	$t0
	add	$sp,	$sp,	-8
	jal	_print_int
	add	$sp,	$sp,	8
	la	$t0,	___str83
	move	$a0,	$t0
	add	$sp,	$sp,	-8
	jal	_print_str
	add	$sp,	$sp,	8
	move	$t0,	$s0
	lw	$t0,	4($t0)
	move	$s0,	$t0
branch_78:
	nop
	j	branch_77
branch_79:
	lw	$s0,	0($sp)
	lw	$ra,	4($sp)
	add	$sp,	$sp,	8
	jr	$ra
_make_list:
	move	$fp,	$sp
	add	$sp,	$sp,	-20
	sw	$s0,	0($sp)
	sw	$s1,	4($sp)
	sw	$s2,	8($sp)
	sw	$s3,	12($sp)
	sw	$ra,	16($sp)
	move	$s0,	$a0
	li	$s2,	0
	li	$s1,	0
branch_91:
	slt	$t0,	$s1,	$s0
	beq	$t0,	$zero,	branch_93
	li	$a0,	8
	add	$sp,	$sp,	-8
	jal	_malloc
	add	$sp,	$sp,	8
	move	$t0,	$v0
	move	$t1,	$t0
	move	$t0,	$t1
	sw	$s1,	0($t0)
	seq	$t0,	$s2,	0
	beq	$t0,	$zero,	branch_100
	move	$s2,	$t1
	move	$s3,	$s2
	j	branch_101
branch_100:
	move	$t0,	$s3
	sw	$t1,	4($t0)
	move	$t0,	$s3
	lw	$t0,	4($t0)
	move	$s3,	$t0
branch_101:
branch_92:
	move	$t0,	$s1
	add	$t0,	$t0,	1
	move	$s1,	$t0
	j	branch_91
branch_93:
	move	$s0,	$s3
	li	$t0,	0
	sw	$t0,	4($s0)
	move	$v0,	$s2
	lw	$s0,	0($sp)
	lw	$s1,	4($sp)
	lw	$s2,	8($sp)
	lw	$s3,	12($sp)
	lw	$ra,	16($sp)
	add	$sp,	$sp,	20
	jr	$ra
main:
	move	$fp,	$sp
	add	$sp,	$sp,	-8
	sw	$s0,	0($sp)
	sw	$ra,	4($sp)
	li	$a0,	10
	add	$sp,	$sp,	-8
	jal	_make_list
	add	$sp,	$sp,	8
	move	$t0,	$v0
	move	$s0,	$t0
	move	$a0,	$s0
	add	$sp,	$sp,	-8
	jal	_print_list
	add	$sp,	$sp,	8
	la	$t0,	___str111
	move	$a0,	$t0
	add	$sp,	$sp,	-8
	jal	_print_str
	add	$sp,	$sp,	8
	move	$t0,	$s0
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
