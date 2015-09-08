from collections import defaultdict
from common import *


# reverse a direted graph
def rev(g):
    new = {}
    for frm, to in g.iteritems(): 
        for t in to:
            new.setdefault(t, []).append(frm)
    return new 


def get_defuse(inst):
    '''
    return def-use info of an instruction
    '''
    if type(inst) != IR:
        return None, []

    opcode, rs, rt, rd = inst
    defed = None
    if is_load(opcode):
        used = [rs] if type(rs) == Register else []
        defed = rt if type(rt) == Register else None
    else:
        defed = rd if type(rd) is Register else None
        used = []
        if type(rs) == Register:
            used.append(rs)
        if type(rt) == Register:
            used.append(rt) 

    return defed, used 


class CFG(object):
    def __init__(self, g, vertices):
        self.g = g
        self.vertices = vertices
        self.rev = rev(g)
        # cache, mapping vertcies -> defuse
        self.defuse = {}

    def inst(self, n):
        return self.vertices[n]

    def __len__(self):
        return len(self.vertices)

    def succ(self, v):
        return self.g[v]

    def pred(self, v):
        return self.rev[v]

    def use(self, v):
        if v in self.defuse:
            return self.defuse[v][1]
        else:
            defuse = get_defuse(self.vertices[v])
            self.defuse[v] = defuse
            return defuse[1]

    def def_(self, v):
        if v in self.defuse:
            return self.defuse[v][0]
        else:
            defuse = get_defuse(self.vertices[v])
            self.defuse[v] = defuse
            return defuse[0] 

    def get_calls(self):
        return (n for n, inst in self.vertices.iteritems()
                if type(inst) == Call)


def make_cfg(insts):
    '''
    make control flow graph from a list of instructions
    ''' 
    g = {}
    # mapping vertices -> insts
    vertices = {}
    # mapping branches -> vertices 
    branches = {}
    # branches or jump insts
    nonlinear_insts = []
    for v, inst in enumerate(insts): 
        vertices[v] = inst
        if type(inst) == IR:
            if inst.opcode in ('j', 'jr'): # unconditional jump
                if inst.opcode == 'j':
                    nonlinear_insts.append(v)
                continue
            elif is_branch(inst.opcode):
                nonlinear_insts.append(v) 
        else: # branches 
            branches[inst] = v
        # normal instructions simply fall through (including branches)
        g.setdefault(v, []).append(v+1) 

    for v in nonlinear_insts: 
        inst = vertices[v]
        jump_target = (inst.rs
                if inst.opcode == 'j'
                else inst.rd)
        g.setdefault(v, []).append(branches[jump_target])

    return CFG(g, vertices) 


def get_lives(cfg):
    '''
    in[n] = use[n] | (out[n] - def[n])
    out[n] = sum of in[s]
        where s is every one of n's successors
    '''
    in_ = defaultdict(set)
    out = defaultdict(set)
    # calculate fixed-point for live-in and live-out variables
    while True:
        num_fixed = 0
        for n in cfg.vertices.keys(): 
            prev_in_len = len(in_[n])
            prev_out_len = len(out[n])
            in_[n] = set(cfg.use(n)) | (out[n] - set([cfg.def_(n)])) 
            for s in cfg.succ(n):
                out[n] |= in_[s] 
            if prev_in_len == len(in_[n]) and prev_out_len == len(out[n]):
                num_fixed += 1
        if num_fixed == len(cfg.vertices.keys()):
            break
    return in_, out 
