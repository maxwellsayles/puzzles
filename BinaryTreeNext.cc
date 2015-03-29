/**
 * Given a binary tree where each node has a `next` pointer, write function to
 * fill out `next` to point to its right sibling on the level.
 *
 * This works by keeping `cur` at one level, while populating `p->next` on the
 * level below.  We iterate using `cur->next` across each row.  Filling out
 * the level below means that `cur->next` is valid by the time we get to that
 * row.  We also keep `head.next` to denote the leftmost node of the next row.
 *
 * The solution is O(n) runtime and O(1) additional space. The input is mutated.
 */

#include <assert.h>

struct Node {
  Node() {}
  Node(Node* l, Node* r, char v): l(l), r(r), v(v) {}
  explicit Node(char v): v(v) {}
  
  Node* l = nullptr;
  Node* r = nullptr;
  Node* next = nullptr;
  char v = '.';
};

void updateNext(Node* root) {
  Node* cur = root;
  while (cur != nullptr) {
    // `head` and `p` refer to the nodes in the row below `cur. `head` is the
    // left most element of the row, while `p` is next node to be updated.
    Node head;
    Node* p = &head;

    while (cur != nullptr) {
      if (cur->l != nullptr) {
	p->next = cur->l;
	p = p->next;
      }
      if (cur->r != nullptr) {
	p->next = cur->r;
	p = p->next;
      }
      cur = cur->next;
    }

    // Move `cur` to the first node of the next row.
    p->next = nullptr;
    cur = head.next;
  }
}

int main(int argc, char* argv[]) {
  Node b('b');
  Node c('c');
  Node a(&b, &c, 'a');
  updateNext(&a);
  assert(a.next == nullptr);
  assert(b.next == &c);
  assert(c.next == nullptr);

  //       g
  //     /   \
  //    a     d
  //   / \   / \
  //  b   c e   f
  Node e('e');
  Node f('f');
  Node d(&e, &f, 'd');
  Node g(&a, &d, 'g');
  updateNext(&g);
  assert(g.next == nullptr);
  assert(a.next == &d);
  assert(d.next == nullptr);
  assert(b.next == &c);
  assert(c.next == &e);
  assert(e.next == &f);
  assert(f.next == nullptr);
  
  //       g
  //     /   \
  //    a     d
  //   / \     \
  //  b   c     f
  d.l = nullptr;
  updateNext(&g);
  assert(g.next == nullptr);
  assert(a.next == &d);
  assert(d.next == nullptr);
  assert(b.next == &c);
  assert(c.next == &f);
  assert(f.next == nullptr);

  //       g
  //     /   \
  //    a     d
  //   /       \
  //  b         f
  a.r = nullptr;
  updateNext(&g);
  assert(g.next == nullptr);
  assert(a.next == &d);
  assert(d.next == nullptr);
  assert(b.next == &f);
  assert(f.next == nullptr);

  //       g
  //     /   \
  //    a     d
  //   / \
  //  b   c
  a.r = &c;
  d.r = nullptr;
  updateNext(&g);
  assert(g.next == nullptr);
  assert(a.next == &d);
  assert(d.next == nullptr);
  assert(b.next == &c);
  assert(c.next == nullptr);
  
  return 0;
}
