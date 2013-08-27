
/**
 * Given a sorted array of integers, 
 * how many unique absolute values are there?
 *
 * e.g. [-1, 0, 1] has only 2 unique absolute values.
 *
 * The algorithm works by tracking indexes right to left, moving
 * the index that points to the larger value, and when a unique
 * value is found, it advances both indexes 'gobbling' up that value.
 *
 * Run time is O(n) and memory is O(1).
 *
 * Compile with g++ -O3 to enable tail call optimizations.
 */

#include <iostream>
#include <stdlib.h>

using namespace std;

// i is the low index
// j is the high index
// z is the largest most recently encountered unique element
// acc is the running count
int rec(const int* A, int i, int j, int z, int acc) {
  int x = abs(A[i]);
  int y = abs(A[j]);

  // one element
  if (i == j) {
    return acc + (x != z);
  }

  // is A[i] the same as the already counted element?
  if (x == z) {
    return rec(A, i+1, j, z, acc);
  }

  // is A[j] the same as the already counted element?
  if (y == z) {
    return rec(A, i, j-1, z, acc);
  }

  // A[i] and A[j] are unique,
  // count the larger one first.
  if (x > y) {
    return rec(A, i+1, j, x, acc+1);
  } else {
    return rec(A, i, j-1, y, acc+1);
  }
}

int count_abs_unique(const int* A, int n) {
  if (n <= 1) return n;
  int i = 0;
  int j = n-1;
  int x = abs(A[i]);
  int y = abs(A[j]);
  if (x > y) {
    return rec(A, i+1, j, x, 1);
  } else {
    return rec(A, i, j-1, y, 1);
  }
}

int main(int argc, char** argv) {
  int A[] = {};
  int B[] = {0};
  int C[] = {0, 1};
  int D[] = {1, 1};
  int E[] = {-1, 1};
  int F[] = {-30, -10, -5, 0, 5, 20, 30};
  cout << count_abs_unique(A, sizeof(A)/sizeof(int)) << endl;
  cout << count_abs_unique(B, sizeof(B)/sizeof(int)) << endl;
  cout << count_abs_unique(C, sizeof(C)/sizeof(int)) << endl;
  cout << count_abs_unique(D, sizeof(D)/sizeof(int)) << endl;
  cout << count_abs_unique(E, sizeof(E)/sizeof(int)) << endl;
  cout << count_abs_unique(F, sizeof(F)/sizeof(int)) << endl;
  return 0;
}
