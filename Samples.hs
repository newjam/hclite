module Samples where

t00 = "int main () { \
\   int c;\n\
\   float b;\n\
\   c = 1;\n\
\   b = -float(c);\n\
\}"
t01 = "int main ( ) {\n\
\    int n, i;\n\
\    float f;\n\
\    n = 3;\n\
\    i = 0;\n\
\    f = 1.0;\n\
\    while (i < n) {\n\
\        i = i + 1;\n\
\        f = f * i;  // implicit (float)i\n\
\    } \n\
\}"
t02 = "int main ( ) {\n\
\    int a, b, c, d;\n\
\    b = a;\n\
\    b = 5;\n\
\    d = c;\n\
\    c = 7;\n\
\}"
t03 = "int main ( ) {\n\
\    float f;\n\
\    int i;\n\
\    char c;\n\
\    c = 'a';\n\
\    i = 77;\n\
\    f = i - int(c);\n\
\}"
t04 = "int main ( ) {\n\
\  n = 3;\n\
\  i = 1;\n\
\  f = 1.0;\n\
\  while (i < n) {\n\
\    i = i + 1;\n\
\    f = f * float(i);\n\
\  }\n\
\}"
t05 = "int main ( ) {\n\
\    int n, i, f;\n\
\    n = 3;\n\
\    i = 1;\n\
\    f = 1;\n\
\    while (i < n) {\n\
\        i = i + 1;\n\
\        f = f * i;\n\
\    }\n\
\}"
t06 = "int main ( ) {\n\
\    float n, i, f;\n\
\    n = 3.0;\n\
\    i = 1.0;\n\
\    f = 1.0;\n\
\    while (i < n) {\n\
\        i = i + 1.0;\n\
\        f = f * i;\n\
\    }\n\
\}"
t07 = "// a first program with\n\
\// two comment lines\n\
\int main() {\n\
\    char c;\n\
\    int i;\n\
\    c = 'h';\n\
\    i = c + 3;\n\
\} // main\n"
t08 = "int main ( ) {\n\
\    float a, b, c, d;\n\
\    a = 5.0; b = 4.5;\n\
\    c = 3.3; d = 2.2;\n\
\    a = (b * float(1)) + (c - d);\n\
\}"
t09 = "int main() {\n\
\  float a, x, result;\n\
\  a = 4.0;\n\
\  x = 1.0;\n\
\  while (x*x > a+0.0001 || x*x < a-0.0001 )\n\
\    x = (x + a/x)/2.0;\n \
\  result = x;\n }"
t10 = "int main() {\n\
\  float a, x, result;\n\
\  a = 9.1;\n\
\  x = 1.0;\n\
\  while (x*x > a+0.0001 || x*x < a-0.0001 )\n\
\    x = (x + a/x)/2.0;\n \
\  result = x;\n }"
t11 = "int main() {\n\
\  int x,y;\n\
\  x = 9;\n\
\  y = 2;\n\
\  if (x>3) { \n\
\  x = x + 1; \n \
\  y = y + 1; \n \
\  } \n \
\  }"
t12 = "int main() {\n\
\  int x,y;\n\
\  x = 1;\n\
\  y = 1;\n\
\  y = y+1;\n \
\  x = y + x; }"
t13 = "int main() {\n\
\  int x,y;\n\
\  x = 1;\n\
\  y = 1;}"
t14 = "int main() {\n\
\  int x,y;\n\
\  x = 1;\n\
\  y = 1;\n\
\  x = y + x; }"
t15 = "int main() {\n\
\  int x,y;\n\
\  x = 1;\n\
\  y = 1;\n\
\  y = y + x; }"
final = "int main () { \n\
\  int i,j; float x,y; \n\ 
\  i = 2; j = 3; x = 4; y = 6; \n\
\  if(x>float(j)) \n\
\    x = y + i; \n\
\  else \n\
\    y = i + j; \n\
\  }"

tests = [t00, t01, t02, t03, t04, t05, t06, t07, t08, t09]



