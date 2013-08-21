/**
 * Reverse foreach on an immutable singly linked list.
 *
 * NOTE: This must be compiled with
 *       g++ -O3 -std=c++11 -o foreach_rev foreach_rev.cc
 *       The -O3 is necessary to enable tail-call optimizations.
 *
 * We present 5 ways of doing this:
 * - O(n) time and O(n) space.
 * - O(n) time and O(sqrt(n)) space.
 * - O(nlglgn) time and O(sqrt(n)) space.
 * - O(nlgn) time and O(lgn) space.
 * - O(n^2) time and O(1) space.
 *
 * The O(n) time and space method simply copies the list into an
 * array and then iterates the array in reverse.
 *
 * The O(n) time and O(sqrt(n)) space method partitions the list into
 * sublists of sqrt(n) elements and then uses the O(n) time and space method
 * on the sublists in reverse order.  This method is interesting because
 * it highlights the technique that if you can subdivide a problem into 
 * sqrt(n) sized pieces and then solve the subproblem using s=sqrt(n) time
 * and space, you have an O(n) time and O(sqrt(n)) space solution.
 *
 * The O(nlglgn) time and O(sqrt(n)) space method similarly partitions the
 * list into sublists of sqrt(n) size.  If the input list is 0 or 1 elements
 * long, then we simply reverse the list.  Otherwise, we recurse.  The
 * complexity analysis of this method is a little trickier.  First, let's
 * consider the time complexity.  Let T(n) be the time for a list of
 * length n, and assume that T(n) <= cnlglgn.  By the description of the
 * algorithm, we have T(n) = n + n^{1/2} T(n^{1/2}).  This holds since
 * in n steps, we can build the sublists.  Then we recurse on n^{1/2}
 * sublists, each of size n^{1/2}.   Substituting our assumption about T(n)
 * gives:
 *   T(n)  = n + n^{1/2} T(n^{1/2})
 *        <= n + n^{1/2} cn^{1/2}lglg(n^{1/2})  (by I.H.)
 *         = n + cnlglg(n^{1/2})
 *         = n + cnlg(lgn/2)
 *         = n + cnlglgn - cn
 *        <= cnlglgn   when c >= 1
 * which proves the runtime.   Now let S(n) be the space for a list of
 * length n.  Assume S(n) <= cn^{1/2}.  By our algorithm, we have:
 *   S(n)  = n^{1/2} + S(n^{1/2})
 *        <= n^{1/2} + cn^{1/4}                 (by I.H.)
 * We want to show that n^{1/2} + cn^{1/4} <= cn^{1/2} for some c.
 *   n^{1/2} + cn^{1/4} <= cn^{1/2}
 *             cn^{1/4} <= cn^{1/2} - n^{1/2}
 *             cn^{1/4} <= (c-1) n^{1/2}
 *          c / (c - 1) <= n^{1/4}
 * which holds for c = 2 and n >= 2^4.  This proves the space complexity.
 *
 * The O(nlgn) time and O(lgn) space method divides the list in half and
 * recursively handles each sublist.  The proof of the complexity is similar
 * to the previous method.
 *
 * Finally, the O(n^2) time and O(1) space iterates the index of the list
 * and repeatedly scans from the head.
 */
#include <functional>
#include <iostream>

#include <math.h>

using namespace std;

struct node {
  node(const int v, const node* t) : value(v), tail(t) {}
  int value;
  const node* tail;
};

/// Recursively delete a list.
/// NOTE: Originally, this was handled by the node dtor, but
/// it wasn't clear that tail call optimization was enabled
/// for recursive deletes, but also, it might not make sense
/// to delete an entire list, just because you delete the head.
void delete_list(const node* xs) {
  if (xs == nullptr) return;
  const node* tl = xs->tail;
  delete xs;
  delete_list(tl);
}

const node* range(const int i, const node* acc = nullptr) {
  if (i <= 0) return acc;
  return range(i - 1, new node(i, acc));
}

int length(const node* hd, const int acc = 0) {
  if (hd == nullptr) return acc;
  return length(hd->tail, acc + 1);
}

const node* drop(const node* hd, const int n) {
  if (hd == nullptr) return nullptr;
  if (n <= 0) return hd;
  return drop(hd->tail, n - 1);
}

// Takes O(n^2) time and O(1) space.
void foreach_rev_n2_1(function<void(int)> f,
		      const node* xs, const int n) {
  for (int i = n - 1; i >= 0; i--) {
    f(drop(xs, i)->value);
  }
}

/// Takes O(nlgn) time and O(lgn) space.
void foreach_rev_nlgn_lgn(function<void(int)> f,
			  const node* xs, const int n) {
  if (n <= 0) {
    return;
  } else if (n == 1) {
    f(xs->value);
    return;
  }

  int k = n / 2;
  foreach_rev_nlgn_lgn(f, drop(xs, k), n - k);
  foreach_rev_nlgn_lgn(f, xs, k);
}

/// Takes O(nlglgn) time and O(sqrt(n)) space.
/// Partitions the list into sqrt(n) sized segments,
/// and recurses.  A list of size 0 or 1 is handled as a base case.
void foreach_rev_nlglgn_sqrtn(function<void(int)> f,
			      const node* xs, const int n) {
  if (n == 0) {
    return;
  } else if (n == 1) {
    f(xs->value);
    return;
  }

  const int s = static_cast<int>(sqrt(n));
  const node* subs[s];
  const node* hd = xs;
  for (int i = 0; i < s; i++) {
    subs[i] = hd;
    hd = drop(hd, s);
  }

  foreach_rev_nlglgn_sqrtn(f, hd, n - s * s);
  
  for (int i = s - 1; i >= 0; i--) {
    foreach_rev_nlglgn_sqrtn(f, subs[i], s);
  }
}

/// Requires O(n) time and O(n) memory.
/// Copies the list into an array and then iterates the array in reverse.
void foreach_rev_n_n(function<void(int)> f,
		     const node* xs, const int n) {
  const node* hd = xs;
  int ys[n];
  for (int i = 0; i < n; i++) {
    ys[i] = hd->value;
    hd = hd->tail;
  }
  for (int i = n - 1; i >= 0; i--) {
    f(ys[i]);
  }
}

/// Uses O(n) time and O(sqrt(n)) memory.
/// Partitions the list into sqrt(n) size segments and then
/// uses foreach_rev_n_n above.
void foreach_rev_n_sqrtn(function<void(int)> f,
			 const node* xs, const int n) {
  const int s = static_cast<int>(sqrt(n));
  const node* subs[s];
  const node* hd = xs;
  for (int i = 0; i < s; i++) {
    subs[i] = hd;
    hd = drop(hd, s);
  }
  foreach_rev_n_n(f, hd, n - s * s);
  for (int i = s - 1; i >= 0; i--) {
    foreach_rev_n_n(f, subs[i], s);
  }
}

int main(int argc, char** argv) {
  const node* xs = range(12345678);
  foreach_rev_n_sqrtn([](int){}, xs, length(xs));
  delete_list(xs);
  return 0;
}
