#include <iostream>
#include <list>
#include <unordered_map>

using namespace std;

class LRU {
public:
  explicit LRU(int cap) : cap_(cap) {}

  void put(int k, int v) {
    if (get(k) == -1) {
      // Key does not exist
      if (data_.size() == cap_) {
        // Remove LRU key and node
        auto head = order_.begin();
        data_.erase(*head);
        nodes_.erase(*head);
	order_.erase(head);
      }
      order_.push_back(k);
    }
    data_[k] = v;
    nodes_[k] = prev(order_.end());
  }

  int get(int k) {
    auto node = nodes_.find(k);
    if (node != nodes_.end()) {
      // Key exists. Make most recently used.
      order_.erase(node->second);
      order_.push_back(k);
      nodes_[k] = prev(order_.end());
      return data_[k];
    }
    return -1;
  }

private:
  int cap_ = 0;
  list<int> order_;
  unordered_map<int, int> data_;
  unordered_map<int, list<int>::iterator> nodes_;
};

int main() {
  LRU cache(3);
  cout << cache.get(1) << endl;
  cache.put(1, 1);
  cache.put(2, 2);
  cache.put(3, 3);
  cache.put(4, 4);
  cout << cache.get(4) << endl;
  cout << cache.get(3) << endl;
  cout << cache.get(2) << endl;
  cout << cache.get(1) << endl;
  cache.put(1, 5);
  cout << cache.get(1) << endl;
  cout << cache.get(4) << endl;

  return 0;
}
