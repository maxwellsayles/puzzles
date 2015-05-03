/**
 * Adds an int to a string using std::accumulate
 * and accumulating over a series of lambdas.
 *
 * This is more of a theoretical exercise.  I would not actually solve
 * this problem this way in a real program.
 *
 * This solution is interesting because the input string is processed
 * from left-to-right.  Each character computes a function that when
 * given a carry value and the corresponding function computed for the
 * character to its left, then computes the solution string to the left
 * of itself.  The result of the std::accumulate is a function that takes
 * the input integer and comptues the result string.
 *
 * Because std::string concatenation takes O(n), this approach takes O(n^2).
 * This is in contrast to the Haskell implementation, which uses difference
 * lists and only takes O(n) (see AddIntToString.hs for additional details).
 *
 * I also provide an implementation that is closer to what I would write
 * if this were to be used in production.  This solution (for_real) takes
 * O(n) to run.
 *
 * To build:  g++ -std=c++11 -o AddIntToString AddIntToString.cc
 */
#include <algorithm>
#include <functional>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>

using namespace std;

string add_int_to_string(const int i, const string& s) {
  function<string(int)> last = [](const int carry) {
    return carry == 0 ? string() : to_string(carry);
  };
  auto step = [](auto k, const char x) {
    return [k, x](const int carry) {
      int y = (carry % 10) + x - '0';
      int carry2 = (carry / 10) + (y / 10);
      return k(carry2) + to_string(y % 10);
    };
  };
  return accumulate(begin(s), end(s), last, step)(i);
}

/// This is how I would actually solve this problem.
string for_real(const int i, const string& s) {
  stringstream res;
  int carry = i;
  for (auto c = s.crbegin(); c != s.crend(); c++) {
    int x = (carry % 10) + *c - '0';
    res << to_string(x % 10);  // Append to the end is O(1).
    carry = (carry / 10) + (x / 10);
  }
  string tmp = res.str();
  reverse(tmp.begin(), tmp.end());  // Reverse is O(n).
  return (carry ? to_string(carry) : "") + tmp;  // O(n).
}

int main(int argc, char** argv) {
  cout << add_int_to_string(1234567, "1999999999999999999999999999999") << endl;
  cout << add_int_to_string(1234567, "0999999999999999999999999999999") << endl;
  cout << add_int_to_string(1234567, "0000000000000000000000000000000") << endl;
  cout << add_int_to_string(1234567, "0") << endl;
  cout << for_real(1234567, "1999999999999999999999999999999") << endl;
  cout << for_real(1234567, "0999999999999999999999999999999") << endl;
  cout << for_real(1234567, "0000000000000000000000000000000") << endl;
  cout << for_real(1234567, "0") << endl;
  return 0;
}
