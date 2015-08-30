import os
import grammar
from grammar import parse, ast
from common import *
import reg_alloc
import flow
from collections import namedtuple

builtin_declrs = ''' 
void print_str(char *s);
void print_int(int n);
'''

# TODO make sure `&arr == arr`

# a value can either be a register or a constant.
# a value can be in memory, regardless of its type 
# e.g. `s.field` can be of type int but with its register
# storing its address in the memory
Value = namedtuple('Value', ['val', 'typ', 'in_mem']) 

# a foor loop
Loop = namedtuple('Loop', ['start', 'cont', 'end']) 


def to_mips_type(ctype):
    if ctype == 'int' or type(ctype) == ast.Pointer:
        return 'word'
    elif ctype == 'char':
        return 'byte' 


def repr_data(data): 
    name, typ, init = data
    return '%s:\t.%s\t%s' % (name, typ, init or 0) 


def repr_register(reg):
    if type(reg) is Register:
        typ, num = reg
        if num is None:
            num = ''
        return '$%s%s' % (typ, num) 
    return reg


def assign(dest, src):
    opcode = 'move' if type(src) == Register else 'li'
    return IR(opcode, rd=dest, rs=src, rt=None) 


# TODO make this comprehensive
def repr_ir(ir):
    '''
    ir formating
    '''
    typ = type(ir)
    if typ == str: # branch
        return ir+':'
    elif typ == IR:
        opcode, rs, rt, rd = ir
        rs = repr_register(rs)
        rt = repr_register(rt)
        rd = repr_register(rd)
        if opcode in ('beq', 'bne', 'blt', 'ble', 'bgt'):
            fields = opcode, rs, rt, rd 
        elif opcode in ('j', 'jal', 'jr'):
            fields = opcode, rs
        elif opcode in ('li', 'move'):
            fields = opcode, rd, rs
        elif is_store(opcode) or is_load(opcode):
            addr = rs if not rs.startswith('$') else "%r(%s)"% (rd, rs)
            return "\t%s\t%s,\t%s"% (opcode, rt, addr)
        else:
            fields = opcode, rd, rs, rt
        return '\t%s\t%s' % (fields[0], ',\t'.join(str(f) for f in fields[1:]))
    else:
        return '\t'+str(ir)


def funcname_to_branch(func):
    '''
    return corresponding branch name of a function
    '''
    return '_' + func if func != 'main' else 'main'


sym_counter = 0 
def gensym(prefix=''):
    global sym_counter
    sym_counter += 1
    return '%s%d' % (prefix, sym_counter)


def binexp_type(left, right):
    larger_type = left if sizeof(left.typ) > sizeof(right.typ) else right
    return larger_type.typ


def new_reg():
    '''
    make a new virutal register
    '''
    return Register('virtual', gensym())


def new_branch():
    '''
    make a new unique branch
    '''
    return gensym('branch_')

def new_loop():
    return Loop(start=new_branch(), cont=new_branch(), end=new_branch())


augmented_assignment = set(['+=', '-=', '*=', '/=', '&=', '|=']) 

def desugar(node):
    ''' 
    * transform while loops into for loops
    * reduce augmented_assignment e.g. turn `a += 1` into `a = a + 1` 
    * transform index into dereferencing e.g turn `a[offset]` into `*(a+offset)`
    '''
    typ = type(node)
    if typ == ast.Declaration:
        return ast.Declaration(node.name, desugar(node.typ))
    elif typ == ast.DeclrAssign:
        return ast.DeclrAssign(node.declr, desugar(node.val))
    elif typ == ast.Function: 
        return ast.Function(
                args=node.args,
                ret=node.ret, 
                body=desugar(node.body))
    elif typ == ast.Block:
        return ast.Block(map(desugar, node.stmts)) 
    elif typ == ast.For:
        return ast.For(
                desugar(node.init),
                desugar(node.cond),
                desugar(node.cont),
                desugar(node.body))
    elif typ == ast.If:
        return ast.If(
                node.cond, 
                desugar(node.conseq),
                desugar(node.alt))
    elif typ == ast.While: 
        return ast.For(
                ast.EmptyStmt(),
                desugar(node.cond),
                ast.EmptyStmt(),
                desugar(node.body)) 
    elif typ == ast.BinExpr:
        left = desugar(node.left)
        right = desugar(node.right)
        if node.op in augmented_assignment:
            op = node.op[0]
            return ast.Assignment(left, ast.BinExpr(op, left, right))
        elif node.op == '=':
            return ast.Assignment(left, right)
        else:
            return ast.BinExpr(node.op, left, right) 
    elif typ == ast.PrefixExpr: 
        if node.op != '++' and node.op != '--':
            return ast.PrefixExpr(node.op, desugar(node.expr))
        else:
            op = '+=' if node.op == '++' else '-='
            return desugar(ast.BinExpr(op, node.expr, grammar.Int(1)))
    elif typ == ast.CallExpr:
        return ast.CallExpr(node.func, map(desugar, node.args))
    elif typ == ast.ChainExpr:
        return ast.ChainExpr(map(desugar, node.exprs))
    elif typ == ast.Index: 
        addr = ast.BinExpr('+', desugar(node.array), desugar(node.index)) 
        return ast.PrefixExpr('*', addr)
    elif typ == ast.PostfixExpr:
        return ast.PostfixExpr(node.op, desugar(node.expr))
    else:
        return node 


bin_opcodes = {
    '+': 'add',
    '-': 'sub',
    '*': 'mul',
    '/': 'div',
    '%': 'rem',
    '>>': 'srl',
    '<<': 'sll',
    '>': 'sgt',
    '>=': 'sge',
    '<': 'slt',
    '<=': 'sle',
    '&': 'and',
    '|': 'or',
    '==': 'seq',
    '!': 'sne'
}
binops = {v: k for k, v in bin_opcodes.iteritems()}


def is_pointer(val):
    return is_ptr_type(val.typ)


def is_ptr_type(typ):
    return type(typ) in (ast.Array, ast.Pointer)


struct_fields = {}
def field_type(struct, field): 
    if struct in struct_fields:
        fields = struct_fields[struct]
    else:
        fields = {f.name.val: f.typ for f in struct.fields}
        struct_fields[struct] = fields
    return fields[field]



def emmit_bin_exp(compiler, exp):
    if exp.op == '&&':
        left = compiler.exp_val(exp.left)
        right_branch = new_branch()
        result = new_reg()
        compiler.emmit_one(IR('beq', rs=left.val, rt=REG_ZERO, rd=right_branch))
        right = compiler.exp_val(exp.right)
        compiler.emmit_one(right_branch)
        compiler.emmit_one(IR('and', rs=left.val, rt=right.val, rd=result))
        return Value(val=result, in_mem=False, typ=binexp_type(left, right)) 
    elif exp.op == '||':
        left = compiler.exp_val(exp.left)
        right_branch = new_branch()
        result = new_reg()
        compiler.emmit_one(IR('bne', rs=left.val, rt=REG_ZERO, rd=right_branch))
        right = compiler.exp_val(exp.right)
        compiler.emmit_one(right_branch)
        compiler.emmit_one(IR('or', rs=left.val, rt=right.val, rd=result))
        return Value(val=result, in_mem=False, typ=binexp_type(left, right)) 
    elif exp.op in bin_opcodes:
        rs = left = compiler.exp_val(exp.left)
        rt = right = compiler.exp_val(exp.right) 
        exp_type = left.typ
        opcode = bin_opcodes[exp.op]
        if is_pointer(left) or is_pointer(right):
            if is_pointer(left): 
                exp_type = left.typ
                ptr = left.val
                idx = right.val
            else:
                exp_type = right.typ
                ptr = right.val
                idx = left.val 
            offset = new_reg()
            compiler.emmit_one(IR(opcode='mul',
                rs=idx,
                rt=sizeof(exp_type.typ),
                rd=offset)) 
            rs_val = offset
            rt_val = ptr
        else:
            rs_val = rs.val
            rt_val = rt.val 
        result = new_reg()
        inst = IR(opcode=opcode,
                rs=rs_val,
                rt=rt_val,
                rd=result)
        compiler.emmit_one(inst) 
        return Value(val=result, in_mem=False, typ=exp_type)
    else:
        # the operator is either '.' or '->'
        # value of the struct will be a register
        # storing addr. of the struct in the memory
        left = compiler.exp(exp.left)
        if type(left.typ) == ast.Pointer and left.in_mem:
            struct_addr = compiler.load(left.val, typ=left.typ)
        else:
            struct_addr = left.val
        if exp.op == '.':
            struct = left.typ
        else: # pointer
            struct = left.typ.typ
        if struct.fields is None:
            # we have to lookup the struct since no struct layout is given
            struct = compiler.scope.lookup(struct.name.val)
        field = exp.right
        assert field.is_('IDENT') 
        offset = offsetof(struct, field.val) 
        field_addr = new_reg()
        compiler.emmit_one(IR(opcode='add', rs=struct_addr, rt=offset, rd=field_addr))
        return Value(val=field_addr, in_mem=True, typ=field_type(struct, field.val))


def emmit_prefix_exp(compiler, exp): 
    # operator can be '*', '&', '-', or '!'
    # ('++' and '--' has been desugared as '+=' ad '-=')
    op = exp.op
    result = new_reg()
    operand = compiler.exp_val(exp.expr) if op != '&' else compiler.exp(exp.expr)
    if op == '!' or op == '-':
        opcode = 'not' if op == '!' else 'neg'
        compiler.emmit_one(IR(
            opcode=opcode,
            rd=result,
            rs=operand.val,
            rt=None))
        return Value(val=result, typ=operand.typ, in_mem=False)
    elif op == '&':
        assert exp.expr.is_('IDENT') # you can only take address of a variable...
        var_name = exp.expr.val
        if operand.in_mem or type(operand.typ) == ast.Array:
            ptr_type = (operand.typ
                    if type(operand.typ) != ast.Array
                    else operand.typ.typ)
            val_type = ast.Pointer(ptr_type)
            if type(operand.val) == Register:
                # since the register already represents a memory address
                # we simply change its annotation data accordingly
                val = operand._replace(typ=val_type, in_mem=False)
            else: # global variable
                compiler.emmit_one(IR('la', rt=result, rs=operand.val, rd=grammar.Int(0)))
                val = Value(val=result, typ=val_type, in_mem=False)
        else:
            # value not in memory,
            # store it in memory and deref
            offset = grammar.Int(compiler.alloc(operand.typ))
            compiler.store(src=operand.val, dest=REG_SP, offset=offset, typ=operand.typ)
            compiler.emmit_one(IR('add', rd=result, rs=REG_SP, rt=offset))
            # update the environment to point out that
            # the value is now in memory
            compiler.scope.add(var_name, Value(val=result, in_mem=True, typ=operand.typ))
            val = Value(in_mem=False, val=result, typ=ast.Pointer(operand.typ))
        return val
    elif op == '*':
        typ = operand.typ.typ
        return operand._replace(in_mem=True, typ=typ)


def emmit_chain_exp(compiler, exp):
    return map(compiler.exp, exp.exprs)[-1]


def emmit_postfix_exp(compiler, exp):
    '''
    emmit instructions for a++ and a--
    '''
    name = exp.expr # MUST be an `IDENT`
    prev_val = new_reg() # value of x++
    compiler.emmit_one(assign(dest=prev_val, src=compiler.exp_val(name).val))
    var = compiler.scope.lookup(name.val) # x itself
    diff = sizeof(var.typ.typ) if is_pointer(var) else 1
    post_val = new_reg()
    compiler.emmit_one(IR('add', rd=post_val, rs=prev_val, rt=grammar.Int(diff)))
    if var.in_mem:
        compiler.store(src=post_val, dest=var.val, typ=var.typ)
    else:
        compiler.emmit_one(assign(dest=var.val, src=post_val))
    return Value(val=prev_val, in_mem=False, typ=var.typ) 


def emmit_call_exp(compiler, exp):
    func_name = exp.func.val
    func = compiler.scope.lookup(func_name) 
    # all functions are called by value
    args = map(compiler.exp_val, exp.args)
    # extra space needed for arguments and return value
    extra = sum(sizeof(get_arg_type(arg)) for arg in func.args) +\
            sizeof(func.ret)
    compiler.emmit_one(SaveRegisters(extra))
    compiler.alloc_args(func, args)
    compiler.emmit_one(IR('jal', rs=funcname_to_branch(func_name), rd=None, rt=None)) 
    ret_val = None
    if func.ret != 'void':
        if fits_register(func.ret):
            reg = new_reg()
            compiler.emmit_one(assign(dest=reg, src=REG_RET))
            ret_val = Value(val=reg, in_mem=False, typ=func.ret)
        else: 
            ret_addr = new_reg()
            # frame will be shrinked to free space of addr and ret val
            # we need to move return value into stack
            stack_offset = compiler.alloc(func.ret)
            compiler.emmit_one(IR('add', rd=ret_addr, rs=REG_SP, rt=stack_offset))
            compiler.memcpy(src=REG_FP, dest=ret_addr, size=sizeof(func.ret))
            ret_val = Value(val=ret_addr, in_mem=True, typ=func.ret)
    compiler.emmit_one(RestoreRegisters(extra))
    return ret_val


# TODO fixed potential bug induced by asisgning a struct to another struct
def emmit_assignment(compiler, assignment):
    '''
    emmit instruction of `a = b`, `*ptr = v`, etc
    '''
    var = compiler.exp(assignment.name)
    val = compiler.exp(assignment.val)
    if val.in_mem: # need to load first
        src = compiler.load(val.val, typ=val.typ)
    else:
        src = val.val
    dest = var.val
    if var.in_mem: # store instead of assign
        if type(src) != Register: # can only store value in register
            reg = new_reg()
            compiler.emmit_one(assign(dest=reg, src=src))
            frm = reg
        else:
            frm = src
        compiler.store(src=frm, dest=dest, typ=var.typ)
    else: # assign
        compiler.emmit_one(assign(dest=dest, src=src))
    return var


def offsetof(typ, field):
    if typ not in layouts:
        sizeof(typ)
    return layouts[typ][field]


def pad(cursize, typ):
    '''
    append a `typ` on to memory of `cursize` 
    get size of padding (which can be 0)

    the memory would look like this
    -----------------------
    | cursize | pad |  |
    -----------------------
    '''
    if type(typ) == ast.Array:
        size = sizeof(typ.typ)
    else:
        size = sizeof(typ)
    if size > 1 and cursize % 4 != 0:
        p = 4 - cursize % 4
    else:
        p = 0
    return p


# mapping struct -> layout (mapping field -> offset)
layouts = {}
def sizeof(typ):
    '''
    return number of bytes of a typ
    '''
    if typ == 'void':
        size = 0
    elif typ == 'int' or type(typ) == ast.Pointer:
        size = 4
    elif typ == 'char':
        size = 1
    elif type(typ) == ast.Struct: 
        size = 0
        layout = {}
        for field in typ.fields:
            offset = size + pad(size, field.typ)
            size = offset + sizeof(field.typ)
            layout[field.name.val] = offset 
        layouts[typ] = layout
        size = mulof4(size)
    elif type(typ) == ast.Array:
        size = sizeof(typ.typ) * typ.cap
    return size


def struct_offset(struct, field):
    offset = 0;
    for declr in struct.fields:
        if declr.name.val == field.val: 
            break
        offset += sizeof(declr.typ)
    return offset


exp_emmitters = {
    ast.Assignment: emmit_assignment,
    ast.BinExpr: emmit_bin_exp,
    ast.PrefixExpr: emmit_prefix_exp,
    ast.PostfixExpr: emmit_postfix_exp,
    ast.ChainExpr: emmit_chain_exp,
    ast.CallExpr: emmit_call_exp,
    ast.EmptyStmt: lambda compiler, _: compiler.emmit_one(NOP)
} 


class Scope(object):
    def __init__(self, parent=None):
        self.parent = parent
        self.env = {}

    def add(self, name, val):
        self.env[name] = val

    def lookup(self, name):
        if name in self.env:
            return self.env[name]
        elif self.parent is not None:
            return self.parent.lookup(name)
        else:
            raise Exception("Unknown variable %s" % name)


def mulof4(n):
    '''
    return a number such that it's
    the smallest number larger or equal than `n` and divisible by 4
    '''
    if n % 4 != 0:
        return 4 - (n % 4) + n 
    else:
        return n


# consider array here
def fits_register(typ):
    return (typ in ('char', 'int') or
            type(typ) == ast.Pointer or
            type(typ) == ast.Array)


def store_regs(regs, offset, insts):
    '''
    batch-store registers on stack
    '''
    for i, reg in enumerate(regs):
        insts.append(IR('sw', rt=reg, rs=REG_SP, rd=offset+i*4))


def load_regs(regs, offset, insts):
    '''
    batch-load registers from stack
    '''
    for i, reg in enumerate(regs):
        insts.append(IR('lw', rt=reg, rs=REG_SP, rd=offset+i*4))


def get_arg_type(arg):
    if type(arg.typ) == ast.Array:
        arg_type = ast.Pointer(arg.typ.typ)
    else:
        arg_type = arg.typ
    return arg_type


# TODO
# abstract all store into a function to make 
# sure constants are loaded into register
# before being stored
class FunctionCompiler(object):
    '''
    generate code for a function
    ''' 
    def __init__(self, func_declr, global_env):
        self.func = func_declr.typ
        self.insts = []
        self.scope = Scope(global_env)
        self.allocated = []
        self.name = func_declr.name.val
        self.declared_strs = []
        # for recursive function
        global_env.add(self.name, func_declr.typ)

    def compile(self):
        '''
        compile a function
        '''
        if self.func.body is None:
            return
        # branch name is the function name prepend with underscore
        self.emmit_many(
            funcname_to_branch(self.name),
            Prolog())
        offset = sizeof(self.func.ret)
        # add arguments into scope
        for i, arg in enumerate(self.func.args):
            arg_type = get_arg_type(arg)
            size = sizeof(arg_type)
            if i < 4 and fits_register(arg_type):
                # arguments passed by register
                reg = new_reg()
                self.emmit_one(assign(dest=reg, src=Register('a', str(i))))
                self.scope.add(arg.name.val, Value(val=reg, in_mem=False, typ=arg_type))
            else:
                # arguments passed by memory
                reg = new_reg()
                self.emmit_one(IR('add', rd=reg, rs=REG_FP, rt=offset))
                self.scope.add(arg.name.val, Value(val=reg, in_mem=True, typ=arg_type)) 
            offset += size

        if self.func.body is not None:
            self.statement(self.func.body) 
            if not type(self.insts[-1]) == Epilog:
                self.emmit_one(Epilog())
        
        reg_alloc.alloc(self)
        self.remove_placeholders()
        self.fold_const()

    def fold_const(self):
        '''
        constant folding;
        this is actually more about producing syntactically correct assembly...
        so that anything like `mul    $t0, 1, 4` will be elliminated....
        '''
        insts = []
        for inst in self.insts:
            if (type(inst) == IR and
                    inst.opcode in binops and
                    type(inst.rs) != Register and
                    type(inst.rt) != Register): 
                left = inst.rs
                right = inst.rt
                op = binops[inst.opcode]
                # my mom should be proud that I am using eval here
                folded = eval('%s %s %s' % (left, op, right))
                inst = IR('li', rd=inst.rd, rs=grammar.Int(folded), rt=None)
            insts.append(inst)
        self.insts = insts


    def remove_placeholders(self):
        ''' 
        replace Prolog/Epilog/SaveRegisters/RestoreRegisters with real instructions
        now that we know what registers are used
        '''
        cfg = flow.make_cfg(self.insts)
        ins, outs = flow.get_lives(cfg)
        # t registers that need to be saved
        tregs = []
        for node in sorted(cfg.get_calls()): 
            tregs.append({reg for reg in outs[node]
                if reg.typ == 't'})
        space = (max(len(regs) for regs in tregs) * 4
                if len(tregs) > 0
                else 0) 
        t_offset = self.alloc(size=space)
        s_offset = self.alloc(size=len(self.sregs)*4+4)
        # replace prologs/epilogs with stores and loads
        insts = []
        if len(tregs) > 0:
            regs = tregs.pop()
        else:
            regs = []
        for inst in self.insts:
            inst_type = type(inst)
            if inst_type == SaveRegisters: 
                store_regs(regs, t_offset, insts)
                insts.append(
                    IR('add', rd=REG_SP, rs=REG_SP, rt=grammar.Int(-mulof4(inst.extra))))
            elif inst_type == RestoreRegisters:
                load_regs(regs, t_offset, insts)
                insts.append(
                    IR('add', rd=REG_SP, rs=REG_SP, rt=grammar.Int(mulof4(inst.extra))))
                if len(tregs) > 0:
                    regs = tregs.pop()
                else:
                    regs = []
            elif inst_type == Prolog:
                # grow stack and store needed s registers
                grow = IR('add',
                        rd=REG_SP,
                        rs=REG_SP,
                        rt=grammar.Int(-self.stack_size()))
                insts.extend([
                        IR('move', rd=REG_FP, rs=REG_SP, rt=None),
                        grow
                        ])
                store_regs(self.sregs+[REG_RA], s_offset, insts)
            elif inst_type == Epilog:
                # restore used registers
                load_regs(self.sregs+[REG_RA], s_offset, insts)
                shrink = IR('add',
                        rd=REG_SP,
                        rs=REG_SP,
                        rt=grammar.Int(self.stack_size()))
                insts.extend([ 
                    shrink,
                    IR('jr', rs=Register('ra', None), rt=None, rd=None)
                    ])
            else:
                insts.append(inst)
        self.insts = insts 

    def new_scope(self):
        self.scope = Scope(self.scope)

    def pop_scope(self):
        self.scope = self.scope.parent

    def alloc_args(self, func, args):
        ''' 
        move arguments (`args` is a list of `Value`s)
        into either registers or memory

        precondition: at this point the stack has been grown
        so that $sp points to base addr of the space
        reserved for saving return value.
        '''
        offset = sizeof(func.ret)
        for i, arg in enumerate(args):
            arg_type = get_arg_type(func.args[i])
            if i < 4 and fits_register(arg_type):
                self.emmit_one(assign(dest=Register('a', str(i)), src=arg.val))
            else: # store argument in memory
                if type(arg.val) != Register: # can only store value in register
                    val = new_reg()
                    self.emmit_one(assign(dest=val, src=arg.val))
                else:
                    val = arg.val
                self.store(src=val, dest=REG_SP, offset=offset, typ=arg_type)
            offset += sizeof(arg_type)

    # TODO load byte by bytes
    def memcpy(self, src, dest, size):
        offset = new_reg() 
        copy_loop = new_branch()
        copy_done = new_branch()
        temp = new_reg()
        word_size = grammar.Int(4)
        self.emmit_many(
            assign(dest=offset, src=REG_ZERO),
            copy_loop,
            IR('bgt', rs=offset, rt=grammar.Int(size), rd=copy_done), 
            IR('lw', rt=temp, rs=src, rd=offset),
            IR('sw', rt=temp, rs=dest, rd=offset),
            IR('add', rd=offset, rs=offset, rt=word_size),
            IR('j', rs=copy_loop),
            loop_one
        )


    def emmit_one(self, inst):
        self.insts.append(inst) 

    def emmit_many(self, *insts):
        self.insts.extend(insts)

    # TODO refactor all emmission of stores to use this function
    def store(self, src, dest, typ, offset=grammar.Int(0)): 
        if typ == 'char':
            opcode = 'sb'
        else:
            opcode = 'sw'
        self.emmit_one(IR(opcode, rt=src, rs=dest, rd=offset))

    def load(self, addr, typ, offset=grammar.Int(0)):
        if typ == 'char':
            opcode = 'lb'
        else:
            opcode = 'lw'
        result = new_reg()
        self.emmit_one(IR(opcode, rt=result, rs=addr, rd=offset))
        return result


    def declare(self, stmt): 
        reg = new_reg()
        if type(stmt) == ast.DeclrAssign:
            declr = stmt.declr
        else:
            declr = stmt
        val = Value(val=reg,
                typ=declr.typ,
                in_mem=False)
        if type(declr.typ) not in (ast.Array, ast.Struct):
            self.scope.add(declr.name.val, val) 
            if type(stmt) == ast.DeclrAssign: 
                self.exp(ast.Assignment(stmt.declr.name, stmt.val))
        else: # struct or array
            if type(declr.typ) == ast.Array:
                arr = declr.typ
                #val = val._replace(typ=ast.Pointer(typ=arr.typ))
                offset = self.alloc(arr)
            else: # struct
                val = val._replace(in_mem=True)
                struct = declr.typ 
                # if no struct specs is provided, we look it up
                # otherwise we record it for later use
                if struct.fields is not None:
                    self.scope.add(struct.name.val, struct)
                else:
                    struct = self.scope.lookup(struct.name.val)
                offset = self.alloc(struct) 
            # map value to mem. addr
            self.emmit_one(IR('add', rd=reg, rs=REG_SP, rt=grammar.Int(offset)))
            self.scope.add(declr.name.val, val) 

    def alloc(self, typ=None, size=None):
        assert type is not None or size is not None
        cursize = self.stack_size()
        padding = pad(cursize, typ) if typ is not None else 0
        size = size if size is not None else sizeof(typ)
        offset = cursize + padding 
        total = padding + size 
        self.allocated.append(total)
        return offset

    def stack_size(self):
        return mulof4(sum(self.allocated))

    def if_(self, stmt, context):
        cond = self.exp(stmt.cond)
        alt_branch = new_branch()
        done_branch = alt_branch if stmt.alt is None else new_branch()
        self.emmit_one(IR('beq', rs=cond.val, rt=REG_ZERO, rd=alt_branch))
        self.statement(stmt.conseq, context)
        self.emmit_one(IR('j', rs=done_branch, rt=None, rd=None))
        if stmt.alt is not None: 
            self.emmit_one(alt_branch)
            self.statement(stmt.alt, context)
        self.emmit_one(done_branch)

    def for_(self, stmt):
        self.exp(stmt.init)
        loop = new_loop()
        self.emmit_one(loop.start)
        cond = self.exp(stmt.cond)
        self.emmit_one(IR('beq', rs=cond.val, rt=REG_ZERO, rd=loop.end))
        self.statement(stmt.body, context=loop)
        self.emmit_one(loop.cont)
        self.exp(stmt.cont)
        self.emmit_many(
            IR('j', rs=loop.start, rt=None, rd=None),
            loop.end
        )

    def statement(self, stmt, context=None):
        typ = type(stmt)
        if typ == ast.If:
            self.if_(stmt, context)
        elif typ == ast.Struct: # struct declaration
            self.scope.add(stmt.name, stmt)
        elif (typ == ast.Declaration or
                typ == ast.DeclrAssign):
            self.declare(stmt)
        elif typ == ast.Block:
            self.new_scope()
            [self.statement(stmt, context) for stmt in stmt.stmts]
            self.pop_scope()
        elif typ == ast.For:
            # since continue/break works on the nearest loop
            # every loop creates a new loop context
            self.for_(stmt)
        elif typ == ast.Break:
            self.emmit_one(IR('j', rs=context.end, rt=None, rd=None)) 
        elif typ == ast.Continue:
            self.emmit_one(IR('j', rs=context.cont, rt=None, rd=None))
        elif typ == ast.Return: 
            self.return_(stmt)
        else:
            self.exp(stmt) 

    def return_(self, stmt):
        if stmt.val is not None:
            result = self.exp(stmt.val)
            ret_type = self.func.ret
            if fits_register(ret_type): 
                val = self.load(result.val, typ=resut.typ) if result.in_mem else result.val
                self.emmit_one(assign(dest=REG_RET, src=val))
            else: 
                size = grammar.Int(sizeof(ret_type))
                dest = new_reg()
                self.memcpy(src=result.reg, dest=REG_FP, size=size)
        self.emmit_one(Epilog()) 

    def exp(self, exp): 
        '''
        emmit instructions for an expression
        and return value representing the expression
        '''
        if exp is None:
            return
        elif type(exp) in exp_emmitters:
            return exp_emmitters[type(exp)](self, exp)
        elif exp.is_('IDENT'):
            name = exp.val
            return self.scope.lookup(name)
        elif exp.typ == 'STRING':
            # need to allocate in text segment 
            str_name = gensym('___str')
            s = new_reg()
            self.declared_strs.append((str_name, exp.val))
            self.emmit_one(IR('la', rt=s, rs=str_name, rd=grammar.Int(0)))
            return Value(val=s, in_mem=False, typ=ast.Pointer('char'))
        else: # plain value 
            return Value(val=exp, in_mem=False, typ=exp.typ)

    def exp_val(self, e):
        '''
        same as exp, load the result if it's in memory
        '''
        v = self.exp(e)
        if v.in_mem:
            return v._replace(in_mem=False, val=self.load(v.val, typ=v.typ))
        else:
            return v


def compile_func(func, global_env, global_declrs):
    c = FunctionCompiler(func, global_env)
    c.compile()
    for str_name, str_val in c.declared_strs: 
        global_declrs.append(Data(str_name, 'asciiz', '"%s"'%str_val))
    return c.insts


def declare_global(scope, name, typ, init=None):
    scope.add(name, Value(val=name, typ=typ, in_mem=True))
    return Data(name=name, typ=to_mips_type(typ), init=init)


# TODO support struct and array literal declaration
def compile_declr(declr, global_env, global_declrs):
    if type(declr) == ast.Declaration:
        typ = declr.typ
        if type(typ) == ast.Function:
            return compile_func(declr, global_env, global_declrs) 
        else:
            return declare_global(global_env, declr.name.val, typ)
    else: # declare with init.  
        declr, init = declr
        return declare_global(global_env, declr.name.val, declr.typ, init.val) 


def compile(src, out):
    insts = []
    declrs = []
    global_env = Scope()
    for stmt in map(desugar, parse(builtin_declrs+src)):
        typ = type(stmt) 
        # there are two types of declaration:
        # delcaration or struct definition
        if typ == ast.Declaration or typ == ast.DeclrAssign:
            code = compile_declr(stmt, global_env, declrs)
            if type(code) == list: 
                insts.extend(code)
            else: # data
                declrs.append(code)
        elif typ == ast.Struct:
            global_env.add(stmt.name.val, stmt)
    out.write('.data\n')
    for declr in declrs:
        out.write(repr_data(declr)+'\n')
    out.write('.text\n')
    for inst in insts:
        out.write(repr_ir(inst)+'\n')
    basedir = os.path.dirname(__file__)
    with open(os.path.join(basedir, 'builtin.s')) as builtin_src:
        out.write(builtin_src.read()) 
