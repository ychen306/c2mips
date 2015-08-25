from compiler import compile


compile('''
        int foo(int x, int y);
        void main() { 
            int x = 0;
            int y = x;
            foo(x, y);
            }''')
