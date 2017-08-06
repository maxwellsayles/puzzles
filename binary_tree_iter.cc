/**
 * Pre, in, and post order binary tree iterators using a stack of lambdas.
 */
#include <cassert>
#include <functional>
#include <iostream>
#include <memory>
#include <stack>

template<typename T>
class btree;

template<typename T>
using btree_ptr = std::unique_ptr<btree<T>>;

template<typename T>
class btree {
public:
  btree(T&& t) : t_(std::forward<T>(t)) {}
  btree(auto&& l, T&& t, auto&& r)
    : l_(std::forward<decltype(l)>(l))
    , t_(std::forward<T>(t))
    , r_(std::forward<decltype(r)>(r)) {}

  btree_ptr<T> l_;
  btree_ptr<T> r_;
  T t_;
};

template<typename T>
auto make_btree(T&& t) {
  return std::make_unique<btree<T>>(std::forward<T>(t));
}

template<typename T>
auto make_btree(auto&& l, T&& t, auto&& r) {
  return std::make_unique<btree<T>>(
    std::forward<decltype(l)>(l),
    std::forward<T>(t),
    std::forward<decltype(r)>(r));
}

template<typename T>
class IterBase {
public:
  virtual ~IterBase() {}

  void init(const auto& t) {
    if (t != nullptr) {
      pushNode(t);
    }
  }

  T next() {
    assert(!stack_.empty());
    auto fnc = stack_.top();
    stack_.pop();
    return fnc();
  }

  bool hasNext() const {
    return !stack_.empty();
  }

protected:
  virtual void pushNode(const btree_ptr<T>& t) = 0;
  std::stack<std::function<T()>> stack_;
};

template<typename T>
class PreOrderIter : public IterBase<T> {
public:
  using IterBase<T>::next;
protected:
  using IterBase<T>::stack_;

  void pushNode(const btree_ptr<T>& t) override {
    assert(t != nullptr);
    if (t->r_ != nullptr) {
      stack_.emplace([this, &t] {
	  pushNode(t->r_);
	  return next();
	});
    }
    if (t->l_ != nullptr) {
      stack_.emplace([this, &t] {
	  pushNode(t->l_);
	  return next();
	});
    }
    stack_.emplace([this, &t] { return t->t_; });
  }
};

template<typename T>
class InOrderIter : public IterBase<T> {
public:
  using IterBase<T>::next;
protected:
  using IterBase<T>::stack_;

  void pushNode(const btree_ptr<T>& t) override {
    assert(t != nullptr);
    if (t->r_ != nullptr) {
      stack_.emplace([this, &t] {
	  pushNode(t->r_);
	  return next();
	});
    }
    stack_.emplace([this, &t] { return t->t_; });
    if (t->l_ != nullptr) {
      stack_.emplace([this, &t] {
	  pushNode(t->l_);
	  return next();
	});
    }
  }
};

template<typename T>
class PostOrderIter : public IterBase<T> {
public:
  using IterBase<T>::next;
protected:
  using IterBase<T>::stack_;

  void pushNode(const btree_ptr<T>& t) override {
    assert(t != nullptr);
    stack_.emplace([this, &t] { return t->t_; });
    if (t->r_ != nullptr) {
      stack_.emplace([this, &t] {
	  pushNode(t->r_);
	  return next();
	});
    }
    if (t->l_ != nullptr) {
      stack_.emplace([this, &t] {
	  pushNode(t->l_);
	  return next();
	});
    }
  }
};

template<typename T>
void iterAndPrint(const auto& t) {
  T iter;
  iter.init(t);
  while (iter.hasNext()) {
    std::cout << iter.next() << ' ';
  }
  std::cout << std::endl;
}

int main(int, char**) {
  auto t = make_btree(make_btree(make_btree(1), 2, make_btree(3)),
  		      4,
  		      make_btree(make_btree(5), 6, make_btree(7)));
  iterAndPrint<PreOrderIter<int>>(t);
  iterAndPrint<InOrderIter<int>>(t);
  iterAndPrint<PostOrderIter<int>>(t);
  return 0;
}
