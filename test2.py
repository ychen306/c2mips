from compiler import compile


compile('''
        int x;
        void main() {
            int *p = &x;
            }''')
