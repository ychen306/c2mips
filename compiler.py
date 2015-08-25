from grammar import parse, ast
import grammar
from common import *
import reg_alloc
from collections import namedtuple
from pprint import pprint


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
    return '%s:\t.%s\t%s' % (name, to_mips_type(typ), init or 0)



def repr_register(reg):
    if type(reg) is Register:
        typ, num = reg
        if num is None:
            num = ''
        return '$%s%s' % (typ, num) 
    return reg


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
        elif opcode in ('j', 'jal'):
            fields = opcode, rs
        elif opcode == 'assign':
            fields = opcode, rd, rs
        elif opcode in ('load', 'store', 'la'):
            fields = opcode, rt, rs, rd
        else:
            fields = opcode, rd, rs, rt
        return '\t%s\t%s' % (fields[0], ',\t'.join(str(f) for f in fields[1:]))
    else:
        return '\t'+str(ir)


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


def is_pointer(val):
    return type(val.typ) == ast.Pointer


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
        struct = compiler.exp(exp.left)
        field = exp.right
        assert field.is_('IDENT') 
        offset = offsetof(struct.typ, field) 
        addr = new_reg()
        compiler.emmit_one(IR(opcode='add', rs=struct.val, rt=offset, rd=addr))
        return Value(val=addr, in_mem=True, typ=field.typ)


def emmit_prefix_exp(compiler, exp): 
    # operator can be '*', '&', '-', or '!'
    # ('++' and '--' has been desugared as '+=' ad '-=')
    op = exp.op
    result = new_reg()
    operand = (compiler.exp_val(exp.expr)
            if op in ('!', '-')
            else compiler.exp(exp.expr))
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
        if operand.in_mem:
            val_type = ast.Pointer(operand.typ)
            if type(operand.val) == Register:
                # since the register already represents a memory address
                # we simply change its annotation data accordingly
                val = operand._replace(typ=val_type, in_mem=False)
            else: # global variable
                compiler.emmit_one(IR('la', rt=result, rs=operand.val, rd=grammar.Int(0)))
                val = Value(val=result, typ=val_type, in_mem=False)
        else:
            # value in register not in memory,
            # store it in memory and deref
            offset = grammar.Int(compiler.alloc(operand.typ))
            compiler.emmit_many(
                IR('store', rs=REG_SP, rd=offset, rt=operand.val),
                IR('add', rd=result, rs=REG_SP, rt=offset))
            # update the environment to point out that
            # the value is now in memory
            compiler.scope.add(var_name, Value(val=result, in_mem=True, typ=operand.typ))
            val = Value(in_mem=False, val=result, typ=ast.Pointer(operand.typ))
        return val
    elif op == '*':
        typ = operand.typ.typ
        if not operand.in_mem:
            return operand._replace(in_mem=True, typ=typ)
        else: # e.g. field `foo` in `struct Bar {int *foo;}`
            result = compiler.load(operand.val)
            return Value(val=result, in_mem=True, typ=typ) 


def emmit_chain_exp(compiler, exp):
    return map(compiler.exp, exp.exprs)[-1]


def emmit_postfix_exp(compiler, exp):
    '''
    emmit instructions for a++ and a--
    '''
    name = exp.expr # MUST be an `IDENT`
    prev_val = new_reg() # value of x++
    compiler.emmit_one(IR('assign', rd=prev_val, rs=compiler.exp_val(name).val, rt=None))
    var = compiler.scope.lookup(name.val) # x itself
    diff = sizeof(var.typ.typ) if is_pointer(var) else 1
    post_val = new_reg()
    compiler.emmit_one(IR('add', rd=post_val, rs=prev_val, rt=grammar.Int(diff)))
    if var.in_mem:
        compiler.store(src=post_val, dest=var.val)
    else:
        compiler.emmit_one(IR('assign', rd=var.val, rs=post_val, rt=None))
    return Value(val=prev_val, in_mem=False, typ=var.typ) 


def emmit_call_exp(compiler, exp):
    func_name = exp.func.val
    func = compiler.scope.lookup(func_name) 
    # all functions are called by value
    args = map(compiler.exp_val, exp.args)
    # extra space needed for arguments and return value
    extra = sum(sizeof(arg.typ) for arg in func.args) +\
            sizeof(func.ret)
    compiler.emmit_one(SaveRegisters(extra))
    compiler.alloc_args(func.ret, args)
    compiler.emmit_one(IR('jal', rs=func_name, rd=None, rt=None)) 
    ret_val = None
    if func.ret not in (None, 'void'):
        if fits_register(func.ret):
            ret_val = Value(val=REG_RET, in_mem=False, typ=func.ret)
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


def emmit_assignment(compiler, assignment):
    '''
    emmit instruction of `a = b`, `*ptr = v`, etc
    '''
    var = compiler.exp(assignment.name)
    val = compiler.exp(assignment.val)
    if val.in_mem: # need to load first
        src = compiler.load(val.val)
    else:
        src = val.val
    dest = var.val
    if var.in_mem: # store instead of assign
        if type(src) != Register: # can only store value in register
            reg = new_reg()
            compiler.emmit_one(IR('assign', rd=reg, rs=src, rt=None))
            frm = reg
        else:
            frm = src
        compiler.emmit_one(IR('store', rt=frm, rs=dest, rd=grammar.Int(0)))
    else: # assign
        compiler.emmit_one(
                IR('assign',
                    rd=dest,
                    rs=src,
                    rt=None))
    return var


# TODO
def offsetof(typ, field):
    pass

# TODO this doensn't consider struct padding
def sizeof(typ):
    '''
    return number of bytes of a typ
    '''
    if typ == 'void':
        return 0
    elif typ == 'int' or type(typ) == ast.Pointer:
        return 4
    elif typ == 'char':
        return 1
    elif type(typ) == ast.Struct:
        return sum(sizeof(declr.typ) for declr in  typ.fields)


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


# consider array here
def fits_register(typ):
    return (typ in ('char', 'int') or
            type(typ) == ast.Pointer) 


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
        # for recursive function
        global_env.add(self.name, func_declr.typ)

    def gen_ir(self):
        if self.func.body is None:
            return
        # branch name is the same function name 
        self.emmit_many(
            self.name,
            Prolog())
        offset = sizeof(self.func.ret)
        # add arguments into scope
        for i, arg in enumerate(self.func.args):
            size = sizeof(arg.typ)
            if i < 4 and fits_register(arg.typ):
                # arguments passed by register
                reg = Register('a', str(i))
                self.scope.add(arg.name.val, Value(val=reg, in_mem=False, typ=arg.typ))
            else:
                # arguments passed by memory
                reg = new_reg()
                self.emmit_one(IR('add', rd=reg, rs=REG_FP, rt=offset))
                self.scope.add(arg.name.val, Value(val=reg, in_mem=True, typ=arg.typ)) 
            offset += size

        if self.func.body is not None:
            self.statement(self.func.body) 
            self.emmit_one(Epilog())
        
        reg_alloc.alloc(self)

    def new_scope(self):
        self.scope = Scope(self.scope)

    def pop_scope(self):
        self.scope = self.scope.parent

    def alloc_args(self, ret_type, args):
        ''' 
        move arguments (`args` is a list of `Value`s)
        into either registers or memory

        precondition: at this point the stack has been grown
        so that $sp points to base addr of the space
        reserved for saving return value.
        '''
        offset = sizeof(ret_type)
        for i, arg in enumerate(args):
            offset += sizeof(arg.typ)
            if i < 4 and fits_register(arg.typ):
                self.emmit_one(IR('assign', rd=Register('a', str(i)), rs=arg.val, rt=None))
            else: # store argument in memory
                if type(arg.val) != Register: # can store value in register
                    val = new_reg()
                    self.emmit_one(IR('assign', rd=val, rs=arg.val, rt=None))
                else:
                    val = arg.val
                self.emmit_one(IR('store', rt=val, rs=REG_FP, rd=offset))

    # TODO load byte by bytes
    def memcpy(self, src, dest, size):
        offset = new_reg() 
        copy_loop = new_branch()
        copy_done = new_branch()
        temp = new_reg()
        word_size = grammar.Int(4)
        self.emmit_many(
            IR('assign', rd=offset, rs=REG_ZERO, rt=None),
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

    # TODO refactor emmission of stores to use this function
    def store(self, src, dest, offset=grammar.Int(0)): 
        self.emmit_one(IR('store', rt=src, rs=dest, rd=offset))

    def load(self, addr, offset=grammar.Int(0)):
        result = new_reg()
        self.emmit_one(IR('load', rt=result, rs=addr, rd=offset))
        return result

    # TODO deal with array and struct literal
    def declare(self, stmt): 
        reg = new_reg()
        if type(stmt) == ast.DeclrAssign:
            declr = stmt.declr
        else:
            declr = stmt
        self.scope.add(declr.name.val,
                Value(val=reg,
                    typ=declr.typ,
                    in_mem=False))
        if type(stmt) == ast.DeclrAssign: 
            self.exp(ast.Assignment(stmt.declr.name, stmt.val))

    def delcare_struct(struct):
        if struct.name is None:
            # anonymous struct, nothing to do
            return
        self.scope.add(struct.name.val, struct) 

    def alloc(self, typ=None, size=None):
        offset = self.stack_size() 
        self.allocated.append(size or sizeof(typ))
        return offset

    def stack_size(self):
        return sum(self.allocated)

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
           if fits_register(result.typ): 
               val = self.load(result.val) if result.in_mem else result.val
               self.emmit_one(IR('assign', rd=REG_RET, rs=val, rt=None))
           else: 
               size = grammar.Int(sizeof(result.typ))
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
        else: # plain value 
            return Value(val=exp, in_mem=False, typ=exp.typ)

    def exp_val(self, e):
        '''
        same as exp, load the result if it's in memory
        '''
        v = self.exp(e)
        if v.in_mem:
            return v._replace(in_mem=False, val=self.load(v.val))
        else:
            return v


def compile_func(func, global_env):
    c = FunctionCompiler(func, global_env)
    c.gen_ir()
    return c.insts


def declare_global(scope, name, typ, init=None):
    scope.add(name, Value(val=name, typ=typ, in_mem=True))
    return Data(name=name, typ=typ, init=init)


# TODO support struct and array literal declaration
def compile_declr(declr, global_env):
    if type(declr) == ast.Declaration:
        typ = declr.typ
        if type(typ) == ast.Function:
            return compile_func(declr, global_env) 
        else:
            return declare_global(global_env, declr.name.val, typ)
    else: # declare with init.  
        declr, init = declr
        return declare_global(global_env, declr.name.val, declr.typ, init.val) 


def compile(src):
    insts = []
    declrs = []
    global_env = Scope()
    for stmt in map(desugar, parse(src)):
        typ = type(stmt) 
        # there are two types of declaration:
        # delcaration or struct definition
        if typ == ast.Declaration or typ == ast.DeclrAssign:
            code = compile_declr(stmt, global_env)
            if type(code) == list: 
                insts.extend(code)
            else: # data
                declrs.append(code)
    print '.data'
    for declr in declrs:
        print repr_data(declr)
    print '.text'
    for inst in insts:
        print repr_ir(inst)
