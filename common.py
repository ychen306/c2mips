from collections import namedtuple

Register = namedtuple('Register', ['typ', 'num']) 
REG_SP = Register('sp', None) # $sp
REG_FP = Register('fp', None) # $fp
REG_RET = Register('v', 0) # $v0
REG_ZERO = Register('zero', None) # $zero
REG_RA = Register('ra', None) # $ra

# quads. ir
# format:
#   jal  {rs} 
#   j    {rs}
#   beq  {rs} {rt} {rd}
#   bne  {rs} {rt} {rd}
#   li   {rd} {rs}
#   move {rd} {rs}
# list of intermediate (fake) opcode 
#     * store -- sw, sh, or sb === R[rs+rd] = R[rt]
#     * load -- lbu, lhu, lui, lw === R[rt] = M[R[rs]+rd]
IR = namedtuple('IR', ['opcode', 'rs', 'rt', 'rd'])
NOP = namedtuple('nop', [])
# IR to store $t registers and allocate extra stack space for arguments before function call
SaveRegisters = namedtuple('SaveRegisters', ['extra'])
# IR to restore registers and deallocate space for arguments after function call 
RestoreRegisters = namedtuple('RestoreRegisters', ['extra'])
# IR to save $s registers and grow stack frame
Prolog = namedtuple('Prolog', [])
# "ir" representing text section
Data = namedtuple('Data', ['name', 'typ', 'init'])
# IR to restore saved register and shrink stack frame and emmit jal 
Epilog = namedtuple('Epilog', [])


store_opcodes = set(['store', 'sw', 'sh', 'sb'])
load_opcodes = set(['load', 'lbu', 'lhu', 'lui', 'lw', 'la', 'lb'])


def is_store(opcode):
    return opcode in store_opcodes


def is_load(opcode):
    return opcode in load_opcodes


branches = set(['beq', 'bne', 'bgt', 'bge', 'blt', 'ble'])
def is_branch(opcode):
    return opcode in branches
