/**
 * Remove 0s from a vector in place.
 * Too easy.
 */

#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

#include <assert.h>

void remove_zeros(std::vector<int>& v) {
  auto i = std::find(v.begin(), v.end(), 0);
  auto j = std::find_if(i, v.end(), [](int x) { return x != 0; });
  while (j != v.end()) {
    std::swap(*i, *j);
    i = std::find(i, v.end(), 0);
    j = std::find_if(j, v.end(), [](int x) { return x != 0; });
  }
  v.resize(std::distance(v.begin(), i));
}

bool test(const std::vector<int>& expected, std::vector<int> test) {
  remove_zeros(test);
  return expected == test;
}

int main(int argc, char* argv[]) {
  assert(test({}, {}));
  assert(test({}, {0}));
  assert(test({}, {0, 0, 0}));
  assert(test({1, 2, 3, 4}, {1, 2, 3, 4}));
  assert(test({1, 2, 3, 4}, {0, 0, 0, 0, 1, 2, 3, 4}));
  assert(test({1, 2, 3, 4}, {1, 2, 3, 4, 0, 0, 0, 0}));
  assert(test({1, 2, 3, 4}, {0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0}));
  assert(test({1, 2, 3, 4}, {0, 1, 0, 2, 0, 3, 0, 4, 0}));
  return 0;
}

