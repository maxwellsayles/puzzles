/**
 * A famous person walks into a room when everyone knows them and they know no
 * one.  Suppose you can only ask if person A knows person B.  How do you find
 * the famous person?
 *
 * The solution here takes O(n) time and O(1) space for n persons.
 *
 * If A knows B, then A is not the celebrity, else B is not the celebrity.
 *
 * Let person 1 be the current candidate. Ask if the candidate knows person 2.
 * If yes, then person 2 is the new candidate, else the candidate remains the
 * same.  Repeat for person 3, 4, ..., n.
 *
 * After n persons, check that everyone knows the candidate and that the
 * candidate knows no one.
 */
#include <algorithm>
#include <boost/optional.hpp>
#include <iostream>
#include <random>
#include <vector>

const std::string RED = "\033[31m";
const std::string GREEN = "\033[32m";
const std::string NORMAL = "\033[0m";

class Room {
public:
  explicit Room(const int n)
      : n(n)
      , adj(n * n, false) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 1);
    std::generate(adj.begin(),
		  adj.end(),
		  [&]() { return static_cast<bool>(dis(gen)); });
    
    auto has_famous_person = static_cast<bool>(dis(gen));
    if (has_famous_person) {
      std::uniform_int_distribution<> disn(0, n - 1);
      int i = disn(gen);
      
      // Everyone knows the famous person.
      // The famous person knows no one.
      for (int j = 0; j < n; j++) {
	adj[j + i * n] = true;
	adj[i + j * n] = false;
      }
    }

    // Everyone knows themself.
    for (int i = 0; i < n; i++) {
      adj[i + i * n] = true;
    }
  }

  bool knows(const int i, const int j) const {
    return adj.at(i + j * n);
  }

  boost::optional<int> find_famous() const {
    // Find the candidate (this is the work horse, right here).
    int candidate = 0;
    for (int i = 1; i < n; i++) {
      if (knows(candidate, i)) {
    	candidate = i;
      }
    }
    
    // Everyone knows the candidate.
    for (int i = 0; i < n; i++) {
      if (!knows(i, candidate)) {
	return boost::none;
      }
    }
    
    // The candidate knows no one (except themself).
    for (int i = 0; i < n; i++) {
      if (knows(candidate, i) && candidate != i) {
	return boost::none;
      }
    }

    return candidate;
  }

  /**
   * Brute force. For each person, check if they are famous.
   */
  boost::optional<int> find_brute() const {
    for (int i = 0; i < n; i++) {
      if (is_famous(i)) {
    	return i;
      }
    }
    return boost::none;
  }
  
private:
  bool is_famous(const int i) const {
    for (int j = 0; j < n; j++) {
      if (i == j) continue;
      if (knows(i, j) || !knows(j, i)) return false;
    }
    return true;
  }
  
  int n;
  std::vector<bool> adj;
};

void test(const int n) {
  Room r(n);
  auto found = r.find_famous();
  auto brute = r.find_brute();
  if (found == brute) {
    if (found) {
      std::cout << GREEN << "OK" << NORMAL << " Found: " << found.get()
		<< std::endl;
    } else {
      std::cout << GREEN << "OK" << NORMAL << " No one famous found."
		<< std::endl;
    }
  } else {
    std::cout << RED << "FAILED!" << NORMAL << " found=" << found << " "
	      << "brute=" << brute << std::endl;
    exit(-1);
  }
}

int main(int argc, char* argv[]) {
  for (int n = 1; n <= 10; n++) {
    for (int round = 0; round < 100; round++) {
      test(10 * n);
    }
  }
  return 0;
}
