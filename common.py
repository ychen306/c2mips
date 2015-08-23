from collections import namedtuple

Register = namedtuple('Register', ['typ', 'num']) 
REG_SP = Register('sp', None) # $sp
REG_FP = Register('fp', None) # $fp
REG_RET = Register('v', 0) # $v0
REG_ZERO = Register('zero', None) # $zero

# quads. ir
# format:
#   jal {rs} 
#   j {rs}
#   beq {rs} {rt} {rd}
#   bne {rs} {rt} {rd}
# list of intermediate (fake) opcode 
#     * assign -- li or move  === R[rd] = rs
#     * store -- sw, sh, or sb === R[rs+rd] = R[rt]
#     * load -- lbu, lhu, lui, lw === R[rt] = M[R[rs]+rd]
IR = namedtuple('IR', ['opcode', 'rs', 'rt', 'rd'])
NOP = IR('nop', rs=None, rt=None, rd=None)
# IR to store registers and allocate extra stack space for arguments before function call
SaveRegisters = namedtuple('SaveRegisters', ['compiler', 'extra'])
# IR to restore registers deallocate stack space after function call 
RestoreRegisters = namedtuple('RestoreRegisters', ['compiler', 'extra'])
# IR to save saved register and grow stack frame
Prolog = namedtuple('Prolog', ['compiler'])
# "ir" representing text section
Text = namedtuple('TextSegment', ['name', 'typ', 'init'])
# IR to restore saved register and shrink stack frame and emmit jal 
Epilog = namedtuple('Epilog', ['compiler'])

store_opcodes = set(['store', 'sw', 'sh', 'sb'])
load_opcodes = set(['load', 'lbu', 'lhu', 'lui', 'lw'])


def is_store(opcode):
    return opcode in store_opcodes


def is_load(opcode):
    return opcode in load_opcodes

branches = set(['beq', 'bne', 'bgt', 'bge', 'blt', 'ble'])
def is_branch(opcode):
    return opcode in branches
