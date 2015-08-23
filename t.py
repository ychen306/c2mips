from reg_alloc import *
from pprint import pprint
import grammar

def Reg(x):
    return Register('virtual', x)

insts = [
    IR('assign', rd=Reg('a'), rs=grammar.Int(0), rt=None),
    'branch1',
    IR('add', rd=Reg('b'), rs=Reg('a'), rt=grammar.Int(1)),
    IR('add', rd=Reg('c'), rs=Reg('c'), rt=Reg('b')),
    IR('mul', rd=Reg('a'), rs=Reg('b'), rt=grammar.Int(2)), 
    IR('blt', rs=Reg('a'), rt=grammar.Int('N'), rd='branch1')
]
cfg = make_cfg(insts)
int_graph = make_int_graph(cfg)
pprint(insts)
