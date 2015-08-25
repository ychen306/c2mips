from compiler import compile

compile('''
        int x;
        int add(int x, int y) {
            return x + y;
            }
        void main() {
            int y = x++; 
            add(x, y);
            }
        ''')
