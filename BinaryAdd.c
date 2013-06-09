#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

int add(int x, int y) {
  while (y) {
    int x1 = x ^ y;         // add each bit (mod 2) in parallel
    int y1 = (x & y) << 1;  // or together each carry from each bit
    x = x1;
    y = y1;
  }
  return x;
}

int main(int argc, char** argv) {
  while (1) {
    int x = rand() - rand();
    int y = rand() - rand();
    printf("%d + %d = %d\n", x, y, add(x, y));
    assert(add(x, y) == x + y);
  }
  return 0;
}
