/**
 * Given two integers, the hamming distance is the number of bit
 * positions where the two integers differ.
 *
 * Given a list of integers, sum the hamming distance for all pairs.
 *
 * We give the O(n^2) trivial solution and an O(n) solution.
 *
 * The trick is to notice that each bit position can be treated in isolation.
 * Knowing the sum of set bits for a given bit position, it is easy to
 * compute the sum of the hamming distance for all pairs for that bit
 * position.
 */

#include <iostream>
#include <vector>

#include <assert.h>
#include <stdlib.h>
#include <time.h>

using namespace std;

const int n = 1000;

int hamming(const int x, const int y) {
  // Count the number of set bits in the xor.
  int z = x ^ y;
  int c = 0;
  while (z != 0) {
    z &= z - 1;
    c++;
  }
  return c;
}

/// Compute the hamming distance for each pair. Takes O(n^2).
int sum_all_pairs_n2(const vector<int>& A) {
  const int n = A.size();
  int sum = 0;
  for (int i = 0; i < n; i++) {
    int x = A[i];
    for (int j = i + 1; j < n; j++) {
      int y = A[j];
      sum += hamming(x, y);
    }
  }
  return sum;
}

/// Each bit position can be treated in isolation.
/// Sum the number of set bits for each bit position in each int.
/// Let this value be h.  The hamming distance for each bit position is
/// h*(n-h).  We sum this for each bit position.  Takes O(n).
int sum_all_pairs_n(const vector<int>& A) {
  const int bits = 8 * sizeof(int);
  const int n = A.size();

  // Compute histogram.
  vector<int> hist(bits, 0);
  for (auto x : A) {
    int bit = 0;
    while (x > 0) {
      if ((x & 1) == 1) {
	hist[bit]++;
      }
      x >>= 1;
      bit++;
    }
  }

  // Sum hamming distance.
  int res = 0;
  for (auto h : hist) {
    res += h * (n - h);
  }
  
  return res;
}

/// Verifies that the O(n) solution is the same as the O(n^2) solution.
/// TODO: It would be good to have some deterministic tests here too.
void test(const vector<int>& A) {
  int a = sum_all_pairs_n2(A);
  int b = sum_all_pairs_n(A);
  assert(a == b);
}

int main(int argc, char** argv) {
  srand(time(0));
  vector<int> A(n);

  cout << "Performing randomized tests on a vector of " << n << " random integers." << endl;
  cout << "This program will never exit." << endl;
  while (true) {
    for (int i = 0; i < n; i ++) {
      A[i] = rand();
    }
    test(A);
    cout << '.' << flush;
  }
  return 0;
}
