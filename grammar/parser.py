from collections import namedtuple
from lex import Lexer
import ast

prim_types = set(['int', 'char', 'void']) 

# TODO rewrite declaration parsing so that something like 
# int a = foo(); can be parsed

#
# Operator precedence according to man page
# 
# 150          () [] -> .                      left to right
# 140          ! ~ ++ -- - (type) * & sizeof   right to left
# 130          * / %                           left to right
# 120          + -                             left to right
# 110          << >>                           left to right
# 100          < <= > >=                       left to right
# 90           == !=                           left to right
# 80           &                               left to right
# 70           ^                               left to right
# 60           |                               left to right
# 50           &&                              left to right
# 40           ||                              left to right
# 30           ?:                              right to left
# 20           = += -= etc.                    right to left
# 10           ,                               left to right 
precedence = {
    'infixl': {
        '.':  150,
        '->': 150,
        '+':  120,
        '-':  120,
        '*':  120,
        '/':  120,
        '%':  120,
        '<<': 110,
        '>>': 110,
        '>=': 100,
        '>':  100,
        '<=': 100,
        '<':  100,
        '==': 90,
        '!=': 90,
        '&':  80,
        '^':  70,
        '|':  60,
        '&&': 50,
        '||': 40,
    }, 
    'infixr': {
        '=':  20,
        '+=': 20,
        '-=': 20,
        '*=': 20,
        '/=': 20,
        '|=': 20,
        '&=': 20,
        '%=': 20
    },
    'prefix': {
        '++': 140,
        '--': 140,
        '*':  140,
        '&':  140,
        '!':  70,
        '-':  70
    },
    'postfix': {
        '++': 140,
        '--': 140
    }
}

values = set(['IDENT', 'INT', 'STRING'])

operators = set(
    [op for op_typ in precedence for op in precedence[op_typ]] +
    ['(', '[', 'sizeof', ',', '{']
) 
operators |= values 


def is_type_modifier(expr): 
    expr_type = type(expr)
    return (expr_type == ast.Index or
        expr_type == ast.PrefixExpr and expr.op == '*')


class LeftBrace(object):
    '''
    parse array/struct initializer
    '''
    lbp = 0 

    def __init__(self, parser):
        self.parser = parser

    def __repr__(self):
        return 'LEFT_BRACE'

    def is_(self, typ):
        return typ == '{'

    def nud(self):
        self.parser.parsing_declr = False
        layout = self.parser.expression()
        if type(layout) == ast.ChainExpr:
            fields = layout.exprs
        else:
            fields = [layout]
        self.parser.parsing_declr = True
        self.parser.expect('}')
        return ast.Layout(fields)


class LeftParen(object):
    lbp = 150

    def __init__(self, parser):
        self.parser = parser

    def __repr__(self):
        return 'LEFT_PAREN'

    def is_(self, typ):
        return typ == '('

    def nud(self):
        # grouping
        expr = self.parser.expression() 
        self.parser.expect(')')
        return expr

    def led(self, left): 
        # function call
        args = [] 
        if not self.parser.parsing_declr and not self.parser.tok.is_(')'):
            args = as_list(self.parser.expression())
        else:
            while not self.parser.accept(')'):
                args.append(self.parser.declaration()) 
                if not self.parser.accept(')'):
                    self.parser.expect(',')
        self.parser.expect(')')
        func_call = ast.CallExpr(func=left, args=args) 
        return func_call 


class LeftBracket(object):
    lbp = 150

    def __init__(self, parser):
        self.parser = parser

    def is_(self, typ):
        return typ == '['

    def led(self, left):
        if self.parser.accept(']'):
            index = None
        else:
            index = self.parser.expression()
        self.parser.expect(']')
        return ast.Index(array=left, index=index)


class Value(object):
    lbp = 0

    def __init__(self, (typ, val)):
        if typ == 'INT':
            typ = 'int'
        self.typ = typ
        self.val = val 

    def __repr__(self):
        return str(self.val)

    def is_(self, typ):
        return self.typ == typ

    def nud(self):
        return self


def as_list(expr):
    if type(expr) == ast.ChainExpr:
        exprs, = expr
    else:
        exprs = [expr]
    return exprs 


class Comma(object):
    lbp = 10

    def __init__(self, parser):
        self.parser = parser

    @property
    def lbp(self):
        return 0 if self.parser.parsing_declr else 10 

    def __repr__(self):
        return 'COMMA'

    def is_(self, typ):
        return typ == ','

    def led(self, left):
        right = self.parser.expression(self.lbp-1) 
        return ast.ChainExpr([left]+as_list(right))
        

class Symbol(object):
    lbp = 0

    def __init__(self, (typ, val)):
        self.typ = typ
        self.val = val

    def __repr__(self):
        return self.typ

    def is_(self, typ):
        return self.typ == typ

    def nud(self):
        return self


class Operator(object):
    def __init__(self, parser, typ):
        self.parser = parser
        self.typ = typ

    def __repr__(self):
        return self.typ

    def is_(self, typ):
        return self.typ == typ
    
    @property
    def lbp(self):
        if self.typ in precedence['prefix']:
            return precedence['prefix'][self.typ]
        elif self.typ in precedence['infixl']:
            return precedence['infixl'][self.typ]
        elif self.typ in precedence['infixr']:
            return precedence['infixr'][self.typ]

    def nud(self): 
        if self.typ not in precedence['prefix']:
            self.parser.error()
        else:
            return ast.PrefixExpr(
                    op=self.typ,
                    expr=self.parser.expression(150))

    def led(self, left):
        if self.typ in precedence['postfix']:
            return ast.PostfixExpr(op=self.typ, expr=left)

        if self.typ in precedence['infixl']:
            rbp = self.lbp
        elif self.typ in precedence['infixr']:
            rbp = self.lbp - 1
        else:
            self.parser.error() 
        return ast.BinExpr(
                op=self.typ,
                left=left,
                right=self.parser.expression(rbp)) 


class Parser(object):
    def __init__(self, prog):
        self.lexer = Lexer(prog)
        self.tokens = self.lexer.lex()
        self.peeked = False
        self.prev = None
        self.tok = self.next_token()
        self.eof = False
        self.parsing_declr = False

    def next_token(self):
        try:
            typ, val = tok = self.tokens.next()
        except StopIteration:
            self.eof = True
            return

        if typ not in operators: 
            return Symbol(tok)
        elif typ == '{':
            return LeftBrace(self)
        elif typ == '(':
            return LeftParen(self)
        elif typ == '[':
            return LeftBracket(self)
        elif typ == ',':
            return Comma(self)
        elif typ in values:
            return Value(tok)
        else:
            return Operator(self, typ) 
    
    def advance(self):
        self.prev = self.tok
        if not self.peeked:
            self.tok = self.next_token()
        else:
            self.peeked = False
            self.tok = self.peekedTok

    def peek(self): 
        self.peeked = True
        self.peekedTok = self.next_token()
        return self.peekedTok

    def error(self, msg=None):
        if msg is None:
            msg = 'Unexpected "%s"' % repr(self.tok)
        raise SyntaxError("Error detected at line %d - %s"% (self.lexer.line, msg))

    def accept(self, typ):
        if self.eof:
            self.error("Unexpected EOF")
        return self.tok.is_(typ)

    def expect(self, typ):
        if not self.accept(typ):
            self.error()
        self.advance()

    def expression(self, rbp=0):
        t = self.tok
        self.advance()
        left = t.nud()
        while rbp < self.tok.lbp:
            t = self.tok
            self.advance()
            left = t.led(left)
        return left

    def parse_declr_expr(self, typ, e):
        '''
        parse an expression as declaration;
    
        e.g. "int *x[8]" when treated as expression will be 
        parsed as `PrefixExpr(op='*', expr=Index(array='x', index=8))`,
        which we want to transform into this:
        `Array(Pointer(typ='int'), cap=8)`
        '''
        while is_type_modifier(e):
            if type(e) == ast.Index:
                cap = int(e.index.val) if e.index is not None else None
                typ = ast.Array(typ=typ, cap=cap)
                e = e.array
            else:
                typ = ast.Pointer(typ=typ)
                e = e.expr
                
        if type(e) == ast.CallExpr: 
            return self.parse_declr_expr(
                    ast.Function(ret=typ or 'void', args=e.args, body=None),
                    e.func)
        else:
            return ast.Declaration(typ=typ, name=e)

    def ctype(self): 
        if self.accept('struct'):
            typ = self.struct()
        elif type(self.tok) == Symbol:
            if self.tok.typ not in prim_types:
                self.error("Invalid type '%s'"% self.tok.typ)
            typ = self.tok.typ
            self.advance()
        else:
            self.error()

        return typ 

    def declaration(self):
        '''
        parse delcaration
        '''
        self.parsing_declr = True
        base_typ = self.ctype()
        if type(base_typ) == ast.Struct and self.accept(';'): 
            return base_typ;
        declr_expr = self.expression()
        val = None
        if type(declr_expr) == ast.ChainExpr:
            self.error("Multi-declaration not supported") 
        if type(declr_expr) == ast.BinExpr:
            if declr_expr.op != '=':
                self.error("Illegal binary operator %s"% declr_expr.op) 
            val = declr_expr.right
            declr_expr = declr_expr.left
        declr = self.parse_declr_expr(base_typ, declr_expr)
        self.parsing_declr = False
        if val is not None:
            return ast.DeclrAssign(declr, val)
        else:
            return declr 

    def is_declaration(self):
        '''
        helper function to tell if a statement is a declaration 
        '''
        is_dec = False
        if self.accept('IDENT'):
            lookahead = self.peek()
            is_dec = lookahead.is_('*') or lookahead.is_('IDENT')
        elif type(self.tok) == Symbol:
            is_dec = self.tok.typ in prim_types or self.tok.typ == 'struct'
        return is_dec 

    def for_(self):
        self.expect('for')
        self.expect('(')
        init = cond = cont = None
        if not self.accept(';'):
            init = self.expression()
            self.expect(';')
        else:
            self.advance()

        if not self.accept(';'):
            cond = self.expression()
            self.expect(';')
        else:
            self.advance()

        if not self.accept(')'):
            cont = self.expression()
            self.expect(')')
        else:
            self.advance()
        body = self.statement()
        return ast.For(init, cond, cont, body)

    def while_(self):
        self.expect('while')
        self.expect('(')
        cond = self.expression()
        self.expect(')')
        body = self.statement()
        return ast.While(cond, body)

    def if_(self):
        self.expect('if')
        self.expect('(')
        cond = self.expression()
        self.expect(')')
        conseq = self.statement()
        alt = None
        if self.accept('else'):
            self.advance()
            alt = self.statement() 
        return ast.If(cond, conseq, alt)

    def parse_function(self, declr): 
        func = declr.typ
        return ast.Declaration(
                typ=ast.Function(
                    ret=func.ret,
                    args=func.args,
                    body=self.block()),
                name=declr.name) 

    def return_(self):
        self.expect('return')
        ret_val = None
        if not self.accept(';'):
            ret_val = self.expression()
        self.expect(';')
        return ast.Return(ret_val)

    def statement(self, is_global=False):
        if self.accept(';'):
            self.advance()
            return ast.EmptyStmt()
        elif self.accept('return'):
            return self.return_()
        elif self.accept('break'):
            self.advance()
            brk = ast.Break()
            self.expect(';')
            return brk
        elif self.accept('continue'):
            self.advance()
            cont = ast.Continue()
            self.expect(';')
            return cont
        elif self.accept('{'):
            return self.block()
        elif self.accept('for'):
            return self.for_()
        elif self.accept('while'):
            return self.while_()
        elif self.accept('if'):
            return self.if_()
        elif self.is_declaration(): 
            declr = self.declaration()
            if (type(declr) == ast.Declaration and
                type(declr.typ) == ast.Function and self.accept('{')):
                return self.parse_function(declr)
            else:
                self.expect(';')
                return declr
        else:
            expr = self.expression()
            self.expect(';')
            return expr

    def block(self):
        self.expect('{')
        stmts = []
        while not self.accept('}'):
            stmts.append(self.statement()) 
        self.expect('}')
        return ast.Block(stmts)

    def arg_list(self):
        self.expect('(')
        args = []
        while not self.accept(')'):
            typ = self.ctype()
            name = self.name()
            args.append(ast.Declaration(name, typ))
            if not self.accept(')'):
                self.expect(',')
        self.expect(')') 
        return args

    def struct(self):
        self.expect('struct')
        name = None
        fields = None
        if self.accept('IDENT'):
            name = self.tok
            self.advance()
        if self.accept('{'):
            fields = []
            self.expect('{')
            while not self.accept('}'): 
                field = self.declaration()
                if type(field) != ast.Declaration:
                    self.error()
                fields.append(field)
                if not self.accept('}'):
                    self.expect(';')
            self.expect('}')
        if name is None:
            self.error("Anonymous sturct unsupported")
        return ast.Struct(name, tuple(fields) if fields is not None else None)

    def program(self): 
        declrs = []
        while not self.eof: 
            declrs.append(self.statement())
        return declrs
