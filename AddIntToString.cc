/**
 * Adds an int to a string using std::accumulate
 * and accumulating over a series of lambdas.
 *
 * To build:  g++ -std=c++11 -o AddIntToString AddIntToString.cc
 */
#include <functional>
#include <iostream>
#include <numeric>
#include <string>

using namespace std;

string add_int_to_string(const int i, const string& s) {
  function<string(int)> last = [](int carry) {
    return carry == 0 ? string() : to_string(carry);
  };
  auto step = [](function<string(int)> k, char x) {
    return [k, x](int carry) {
      int y = (carry % 10) + x - '0';
      int carry2 = (carry / 10) + (y / 10);
      return k(carry2) + to_string(y % 10);
    };
  };
  return accumulate(s.begin(), s.end(), last, step)(i);
}

int main(int argc, char** argv) {
  cout << add_int_to_string(1, "1999999999999999999999999999999") << endl;
  cout << add_int_to_string(1, "0999999999999999999999999999999") << endl;
  cout << add_int_to_string(1, "0000000000000000000000000000000") << endl;
  cout << add_int_to_string(1234567, "0") << endl;
  return 0;
}
