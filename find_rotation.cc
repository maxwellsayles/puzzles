/**
 * Given a sorted vector that has been rotated by j places, determine j.
 * The algorithm recursively looks at the two end points and midpoint of the
 * subproblem.  If the end points of the left half are increasing, then recurse
 * on the right half, otherwise recurse on the left half.
 *
 * The runtime is O(logn) and space complexity is O(1) with tail call
 * optimizations turned on.
 */

#include <iostream>
#include <vector>

#include <assert.h>

using namespace std;

int find_rotation(const vector<int>& A, const int i, const int j) {
  if (i >= j) return 0;
  if (i == j - 1) return A[i] < A[j] ? 0 : j;
  const int m = (i + j) / 2;
  return A[i] < A[m] ? find_rotation(A, m, j) : find_rotation(A, i, m); 
}

int main(int argc, char** argv) {
  const int N = 1000;
  cout << "Testing all vectors 1..n for n=1 through " << N - 1 << endl;
  cout << "on every possible rotation for each vector." << endl;
  cout << "This may take a while." << endl;
  for (int n = 1; n < N; n++) {
    vector<int> A(n);
    for (int j = 0; j < n; j++) {  // rotation
      for (int i = 0; i < n; i++) {  // index
	A[i] = (i + j) % n;
      }
      assert(find_rotation(A, 0, n-1) == (n - j) % n);
    }
  }
  return 0;
}
