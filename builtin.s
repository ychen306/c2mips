_print_str:
	li	$v0,	4
	syscall
	jr	$ra 
_print_int: 
	li	$v0,	1
	syscall
	jr	$ra
