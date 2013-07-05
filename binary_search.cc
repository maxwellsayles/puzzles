/**
 * This implements a binary search for the left/right most target value.
 * The result is in the corresponding index if the value is found, and -1
 * otherwise.
 */

#include <iostream>

using namespace std;

// If the probe is equal to x, we continue on the left half.
int left_search(const int* A, const int n, const int x) {
  int m = n;
  int i = 0;
  while (m > 1) {
    int k = m / 2;
    if (A[i + k - 1] < x) {  // Check the right most index of the left half.
      // Continue on the right half.
      i += k;
      m -= k;
    } else {
      // Continue on the left half.
      m = k;
    }
  }
  if (m == 0) return -1;
  return A[i] == x ? i : -1;
}

// If the probe is equal to x, we continue on the right half.
int right_search(const int* A, const int n, const int x) {
  int m = n;
  int i = 0;
  while (m > 1) {
    int k = m / 2;
    if (A[i + k] > x) {  // Check the left most index of the right half.
      // Continue on the left half.
      m = k;
    } else {
      // Continue on the right half.
      i += k;
      m -= k;
    }
  }
  if (m == 0) return -1;
  return A[i] == x ? i : -1;
}

int main(int argc, char** argv) {
  int A[] = {1, 3, 3, 3, 3, 5};

  cout << left_search(A, 6, 0) << endl;
  cout << left_search(A, 6, 1) << endl;
  cout << left_search(A, 6, 3) << endl;
  cout << left_search(A, 6, 5) << endl;
  cout << left_search(A, 6, 6) << endl;
  cout << left_search(nullptr, 0, 0) << endl;

  cout << right_search(A, 6, 0) << endl;
  cout << right_search(A, 6, 1) << endl;
  cout << right_search(A, 6, 3) << endl;
  cout << right_search(A, 6, 5) << endl;
  cout << right_search(A, 6, 6) << endl;
  cout << right_search(nullptr, 0, 0) << endl;

  return 0;
}
