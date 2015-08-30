from collections import namedtuple

# if fields is None then it's referring to a defined named struct
Struct = namedtuple('Struct', ['name', 'fields'])
Pointer = namedtuple('Pointer', ['typ'])
Array = namedtuple('Array', ['typ', 'cap'])

# e.g. {1,2,3}
Layout = namedtuple('Layout', ['fields'])

Sizeof = namedtuple('Sizeof', ['operand'])

BinExpr = namedtuple('BinaryExpression', ['op', 'left', 'right'])
PrefixExpr = namedtuple('PrefixExpr', ['op', 'expr'])
PostfixExpr = namedtuple('PostfixExpr', ['op', 'expr'])
CallExpr = namedtuple('FunctionCall', ['func', 'args'])
ChainExpr = namedtuple('ChainExpression', ['exprs'])
Index = namedtuple('Index', ['array', 'index']) 

For = namedtuple('For', ['init', 'cond', 'cont', 'body'])
While = namedtuple('While', ['cond', 'body'])
If = namedtuple('If', ['cond', 'conseq', 'alt'])
Block = namedtuple('Block', ['stmts']) 
Function = namedtuple('Function', ['ret', 'args', 'body'])
Declaration = namedtuple('Declaration', ['name', 'typ']) 
DeclrAssign = namedtuple('DeclareAndAssign', ['declr', 'val'])
Assignment = namedtuple('Assignment', ['name', 'val'])
Return = namedtuple('Return', ['val'])

class EmptyStmt(object): pass
class Break(object): pass
class Continue(object): pass
