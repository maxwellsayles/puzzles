#include <algorithm>
#include <functional>
#include <future>
#include <iostream>
#include <vector>

template<typename T>
std::vector<T> pmap(const std::vector<T>& xs,
		    std::function<T(T)> f) {
  std::vector<std::future<T>> futures;
  for (const T& x : xs) {
    futures.emplace_back(std::async(f, x));
  }
  std::vector<T> res;
  for (auto& f : futures) {
    res.emplace_back(f.get());
  }
  return res;
}

long squared_sum(long n) {
  return (n * (n + 1) * (2 * n + 1)) / 6;
}

int main(int argc, char* argv[]) {
  long n = 10000;
  std::vector<long> bs;
  for (long i = 0; i < n; i++) {
    bs.emplace_back(i);
  }
  auto xs = pmap<long>(bs, [](long x) { return x * x; });
  auto sum = std::accumulate(xs.begin(), xs.end(), 0UL);
  std::cout << sum << std::endl;
  std::cout << squared_sum(n) << std::endl;;
  return 0;
}
