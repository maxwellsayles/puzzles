/*
You are given the future prices of two stocks GOOG and AAPL, but you can
only hold at most 1 share total (i.e. either GOOG or AAPL, but not both),
and you can only do 1 transaction a day (you can either sell or buy, but
not both).  You want to earn as much money as possible.  How?

NOTE: It's not necessary to make a transaction every day, nor is it
necessary to hold a share everyday.


Define:
fa(i) = We own A at the end of day
fb(i) = We own B at the end of day
f0(i) = We own nothing at the end of day
where each is the maximum paper earnings (includes both cash and market value)

fa(i) = max(f0(i-1), fa(i-1) + pa[i] - pa[i-1])
fb(i) = max(f0(i-1), fb(i-1) + pb[i] - pb[i-1])
f0(i) = max(f0(i-1), fa(i-1) + pa[i] - pa[i-1], fb(i-1) + pb[i] - pb[i-1])
return f0(n)

or:

fa(i) = we sold A on day i
fb(i) = we sold B on day i
f0(i) = we had nothing to sell on day i

fa(i) = max_j pa[i] - pa[j] + Opt(j-1)
fb(i) = max_j pb[i] - pb[j] + Opt(j-1)
f0(i) = Opt(i-1)
Opt(i) = max { fa(i), fb(i), f0(i) }
return Opt(n)
*/

#include <stdlib.h>
#include <time.h>

#include <algorithm>
#include <iostream>
#include <limits>
#include <vector>
#include <utility>

using namespace std;

int dp0(const vector<int>& pa, const vector<int>& pb) {
  int f0 = 0;
  int fa = 0;
  int fb = 0;
  for (int i = 1; i < pa.size(); ++i) {
    fa = max(f0, fa + pa[i] - pa[i-1]);
    fb = max(f0, fb + pb[i] - pb[i-1]);
    f0 = max(fa, fb);
  }
  return f0;
}

int dp1(const vector<int>& pa, const vector<int>& pb) {
  vector<int> opt;
  opt.push_back(0);
  int fa = 0;
  int fb = 0;
  for (int i = 1; i < pa.size(); ++i) {
    fa = pa[i] - pa[0];
    for (int j = 1; j < i; ++j) {
      fa = max(fa, pa[i] - pa[j] + opt[j-1]);
    }

    fb = pb[i] - pb[0];
    for (int j = 1; j < i; ++j) {
      fb = max(fb, pb[i] - pb[j] + opt[j-1]);
    }

    opt.push_back(max(opt[i-1], max(fa, fb)));
  }
  return *opt.rbegin();
}

int main(int argc, char** argv) {
  srand(time(0));
  const int n = 10000;
  vector<int> pa;
  vector<int> pb;
  for (int i = 0; i < n; ++i) {
    pa.push_back(rand() % 100);
    pb.push_back(rand() % 100);
  }
  cout << dp0(pa, pb) << endl;
  cout << dp1(pa, pb) << endl;
  return 0;
}
