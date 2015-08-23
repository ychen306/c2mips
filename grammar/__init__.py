from parser import Parser, Value
import ast
from functools import partial

def Int(n): 
    return Value(('INT', n)) 

def parse(prog): 
    return Parser(prog).program()

def function(func_str):
    return Parser(func_str).function()
