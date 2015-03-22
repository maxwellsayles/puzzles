#include <assert.h>
#include <iostream>
#include <string>


bool greedy_match(const std::string& regex,
		  const std::string& sample,
		  const int i,
		  const int j) {
  const auto n = regex.size();
  const auto m = sample.size();
  if (i == n && j == m) return true;
  if (i == n) return false;
  if (j == m) {
    if (i + 1 < n && regex[i + 1] == '*') {
      return greedy_match(regex, sample, i + 2, j);
    } else {
      return false;
    }
  }
  if (i + 1 < n && regex[i + 1] == '*') {
    if (regex[i] == sample[j]) {
      return greedy_match(regex, sample, i, j + 1);
    } else {
      return greedy_match(regex, sample, i + 2, j);
    }
  }
  if (regex[i] != sample[j]) return false;
  return greedy_match(regex, sample, i + 1, j + 1);
}

std::string rewrite(const std::string& regex) {
  if (regex.size() <= 1) return regex;
  
  std::string a(1, regex[0]);

  // a+ => aa*
  if (regex[1] == '+') {
    return a + rewrite(a + "*" + regex.substr(2));
  }

  if (regex.size() == 2) return regex;

  // a*a* => a*
  if (regex.size() >= 4 &&
      regex[0] == regex[2] &&
      regex[1] == '*' &&
      regex[3] == '*') {
    return rewrite(regex.substr(2));
  }

  // a*a => aa*
  if (regex[1] == '*' && regex[0] == regex[2]) {
    return a + rewrite(a + "*" + regex.substr(3));
  }

  return a + rewrite(regex.substr(1));
}

bool match(const std::string& regex, const std::string& sample) {
  return greedy_match(rewrite(regex), sample, 0, 0);
}

int main(int argc, char* argv[]) {
  assert(match("", ""));
  assert(match("a", "") == false);
  assert(match("", "a") == false);
  assert(match("a", "a"));
  assert(match("ab", "ab"));
  assert(match("ba", "ab") == false);

  assert(rewrite("a+") == "aa*");

  assert(match("a+", "a"));
  assert(match("a+", "aa"));
  assert(match("a+", "aaa"));
  assert(match("a+", "ab") == false);
  assert(match("a+a", "aaa"));
  assert(match("aa+", "aaa"));
  assert(match("a*b+", "b"));
  assert(match("a*b+", "ab"));
  assert(match("a*b+", "bb"));
  assert(match("a*b+", "abb"));
  assert(match("a*b+", "aabb"));
  assert(match("a*b+", "aaabb"));

  assert(match("a*", ""));
  assert(match("a*", "a"));
  assert(match("a*", "aa"));
  assert(match("a*", "ab") == false);
  
  assert(match("a*a*", ""));
  assert(match("a*a*", "a"));
  assert(match("a*a*", "aa"));
  assert(match("a*a*", "ab") == false);

  assert(match("a*b*a", "a"));

  return 0;
}
