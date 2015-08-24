from compiler import compile

compile('''
        int add(int x, int y) {
            return x + y;
            }
        void main() {
            int x = 0;
            int y =x; 
            add(x, y);
            }
        ''')
