/**
 * Let 1=A, 2=B, 3=C, 10=J, ..., 26=Z. Given a string of digits,
 * how many ways can it be translated to letters.
 *
 * For example, 218 can be translated as 'BAH' or 'UH' or 'BR'.
 *
 * The solution here is DP of O(n) time and O(1) space.
 *
 * If the current character is 1-9, then Opt[i] includes Opt[i-1].
 * If the previous character and the current character are 10-26,
 * then Opt[i] includes Opt[i-2].  If the current character is 0,
 * then the previous character must be 1 or 2.  Only characters 0-9
 * are valid.
 */
#include <iostream>

#include <assert.h>

int solve(const std::string& input) {
  int n = input.size();

  // Base cases.
  int x0 = 1;
  int x1 = 1;
  if (input[0] < '1' || input[0] > '9') {
    return 0;
  }

  // Inductive step.
  for (int i = 1; i < n; ++i) {
    // Sanity check.
    if (input[i] < '0' || input[i] > '9') {
      return 0;
    }
    // If the current character is '0', only '10' and '20' are valid.
    if (input[i] == 0 && (input[i-1] != '1' && input[i-1] != '2')) {
      return 0;
    }
    
    int x = input[i] == '0' ? 0 : x0;

    // Two character version.
    if (input[i-1] == '1') {
      x += x1;
    } else if (input[i-1] == '2' && input[i] <= '6') {
      x += x1;
    }

    x1 = x0;
    x0 = x;
  }

  return x0;
}

int main(int argc, char* argv[]) {
  assert(solve("") == 0);
  assert(solve("0") == 0);
  assert(solve("a") == 0);
  assert(solve("1a") == 0);
  assert(solve("10a") == 0);
  assert(solve("11a") == 0);
  assert(solve("1a1") == 0);
  assert(solve("30") == 0);

  assert(solve("1") == 1);
  assert(solve("9") == 1);
  assert(solve("10") == 1);
  assert(solve("20") == 1);
  assert(solve("101") == 1);
  assert(solve("201") == 1);
  assert(solve("1010") == 1);
  assert(solve("2010") == 1);
  assert(solve("1201") == 1);
  
  assert(solve("11") == 2);
  assert(solve("19") == 2);
  assert(solve("21") == 2);
  assert(solve("26") == 2);
  assert(solve("131") == 2);
  assert(solve("1110") == 2);
  
  assert(solve("111") == 3);
  assert(solve("121") == 3);
  assert(solve("218") == 3);

  assert(solve("2121") == 5);
  
  return 0;
}
