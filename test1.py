# test emmitter for various expressions
from compiler import *



compile('''
        int x = 0;
        int add(int a, int b) {
            return a + b;
            }
        void main() {
            int i = 0;
            int evens = 0;
            int odds = 0;
            for (; i < 100; x++, i++){
                if (i % 2 == 0) {
                        evens = add(evens ,i);
                    } else {
                        odds = add(odds, i);
                    }
                    int foo = x++;
                }
            }
        ''')


