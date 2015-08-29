from compiler import *
s = '''
struct Foo {
    int a; 
    char b;
    char c;
    char d;
    int e;
};
'''
# 1111|1|1|1|0|1111
print sizeof(parse(s)[0])
print layouts
