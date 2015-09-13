import re

__all__ = ['Lexer']

reserved = {
        'void', 
        'char',
        'int',
        'float',
        'for',
        'while',
        'break',
        'continue',
        'if',
        'struct',
        'sizeof',
        'typedef',
        'return',
        'else',
        'NULL' # the compiler is not written to work with CPP, so we basically manually translate NULL to 0
        }

op_chars = {'&', '|', '+', '-', '*', '/', '>', '<', '%', '=', '!'}


class Lexer(object):
    def __init__(self, input):
        self.input = input
        self.pos = 0
        self.c = input[0]
        self.line = 1
        self.eof = False

    def advance(self, offset=1):
        self.pos += offset
        if self.pos < len(self.input):
            self.c = self.input[self.pos]
            if self.c == '\n':
                self.line += 1
        else:
            self.eof = True

    def error(self, msg=None):
        if msg is None:
            msg = "Unexpected %s at line %d" % repr(self.ch()), self.line 
        raise SyntaxError(msg)

    def ch(self):
        if self.eof:
            self.error("Unexpected EOF")
        return self.c

    def accept(self, ch):
        return self.ch() == ch 

    def expect(self, c):
        if not self.accept(c):
            self.error()
        else:
            self.advance() 

    def peek(self):
        if self.eof:
            self.error()
        return self.input[self.pos+1]

    def number(self):
        val = ''
        typ = 'INT'
        while self.ch().isdigit(): 
            val += self.ch()
            self.advance()
        return typ, val 

    def string(self):
        s = ''
        self.expect('"')
        while not self.accept('"'):
            if self.accept('/') and self.peek() == '"':
                s += '/"'
                self.advance(2)
            else:
                s += self.ch()
                self.advance()
        self.expect('"')
        return 'STRING', s 

    def line_comment(self):
        self.expect('/')
        self.expect('/')
        while not self.accept('\n') or self.accept('\r'):
            self.advance()

    def block_comment(self):
        self.expect('/')
        self.expect('*')
        while not (self.accept('*') and self.peek() == '/'):
            self.advance()
        self.expect('*')
        self.expect('/')

    def ident(self):
        s = ''
        while self.ch().isalnum() or self.ch() == '_':
            s += self.ch()
            self.advance()
        if s in reserved:
            if s == 'NULL':
                return 'INT', 0
            else:
                return s, None
        else:
            return 'IDENT', s

    def char(self):
        self.expect('\'')

    def operator(self):
        op = ''
        if self.ch() == '*' and self.peek() != '=':
            self.advance()
            return '*'
        while self.ch() in op_chars:
            op += self.ch()
            self.advance()
        return op

    def lex(self):
        while not self.eof:
            ch = self.ch()
            if ch.isspace():
                self.advance()
            elif ch.isalpha() or ch == '_':
                yield self.ident()
            elif ch == '\'':
                yield self.char()
            elif ch == '"':
                yield self.string()
            elif ch == '/' and self.peek() == '/':
                self.line_comment()
            elif ch == '/' and self.peek() == '*':
                self.block_comment()
            elif ch.isdigit():
                yield self.number()
            elif ch == '-' and self.peek() == '>':
                yield '->', None
                self.advance(2)
            elif ch in op_chars:
                op = self.operator()
                yield op, None
            else:
                yield ch, None 
                self.advance()
