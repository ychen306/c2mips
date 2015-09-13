from common import *
import flow 


def get_one(col):
    return next(iter(col))


def color(g, k):
    '''
    optimistic coloring
    precondition: `g` is a graph where keys are vertices
    and values are sets of vertices neighboring keys
    '''
    vs = set(g.keys())
    stack = []
    removed = set()
    while len(vs) > 0:
        for v in vs: 
            if len(g[v] - removed) < k:
                break 
        vs.remove(v)
        removed.add(v)
        stack.append(v) 

    colors = set(range(k))
    coloring = {}
    uncolored = []
    while len(stack) > 0:
        v = stack.pop()
        removed.remove(v) 
        used_colors = {coloring[neighbor] for neighbor in (g[v] - removed)} 
        if len(used_colors) >= k:
            uncolored.append(v)
            coloring[v] = None
        else:
            coloring[v] = get_one(colors - used_colors)
    return coloring, uncolored


def make_int_graph(cfg):
    liveouts = flow.get_lives(cfg)
    g = {}
    for n in cfg.vertices.keys():
        inst = cfg.inst(n)
        if type(inst) != IR:
            continue
        defed = cfg.def_(n)
        uninterfered = [defed]
        if defed is None:
            continue
        if inst.opcode == 'move':
            uninterfered.append(inst.rs)
        if defed.typ != 'virtual':
            continue
        if defed not in g:
            g[defed] = set()
        for reg in liveouts[n]:
            if reg in uninterfered or reg.typ != 'virtual':
                continue
            g[defed].add(reg)
            if reg not in g:
                g[reg] = set()
            g[reg].add(defed)

    return g, liveouts

def alloc(compiler):
    '''
    given a function compiler, allocate registers for its registers
    '''
    # only use temporary registers for now
    k = 18 # s0 - s7 and t0 - t9
    insts = compiler.insts
    cfg = flow.make_cfg(insts)
    int_graph, liveouts = make_int_graph(cfg)
    coloring, uncolored = color(int_graph, k) 
    while len(uncolored) > 0: # have to spill 
        spilled = set(uncolored)
        new_insts = []
        addrs = {}
        for reg in uncolored:
            addrs[reg] = compiler.alloc(size=4)
        for inst in insts:
            defed, used = flow.get_defuse(inst)
            for reg in used:
                if reg in spilled: # load spilled from memory 
                    new_insts.append(IR('lw', rt=reg, rs=REG_SP, rd=addrs[reg]))
            new_insts.append(inst)
            if defed in spilled: # store def
                new_insts.append(IR('sw', rt=defed, rs=REG_SP, rd=addrs[defed]))
        insts = new_insts
        cfg = flow.make_cfg(insts)
        int_graph, liveouts = make_int_graph(cfg)
        coloring, uncolored = color(int_graph, k) 

    # keep register living across function calls
    # in saved register and the rest in temporarys 
    calls = cfg.get_calls()
    # colors that should be mapped in saved registers
    saved = {coloring[reg] for call in calls
            for reg in liveouts[call]
            if reg in coloring}
    # there are only 8 saved registers
    # we need to use some t registers to hold values that
    # live across function calls
    saved_tregs = []
    while len(saved) > 8:
        saved_tregs.append(saved.pop())
    # $s0 - $s7
    sregs = [Register('s', i) for i in range(7+1, -1, -1)]
    # $t0 - $t9
    tregs = [Register('t', i) for i in range(9+1, -1, -1)] 
    # mapping: colors -> registers
    translations = {
            color: sregs.pop() if color in saved else tregs.pop()
            for color in set(coloring.values())} 
    # replace virtual registers with physical registers
    compiler.insts = []
    for inst in insts: 
        if type(inst) == IR:
            opcode, rs, rt, rd = inst
            if rs in coloring:
                rs = translations[coloring[rs]]
            if rt in coloring:
                rt = translations[coloring[rt]]
            if rd in coloring:
                rd = translations[coloring[rd]]
            inst = IR(opcode, rs, rt, rd)
            if any(type(u) == Register and u.typ == 'virtual' for u in (rs, rt)):
                # if there's any virtual registers left
                # it means it's a dead value hence the instruction is dead code
                continue
        compiler.insts.append(inst)
    compiler.sregs = [translations[c] for c in saved]
