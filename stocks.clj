(comment
 "You are given the future prices of two stocks GOOG and AAPL, but you can
  only hold at most 1 share total (i.e. either GOOG or AAPL, but not both),
  and you can only do 1 transaction a day (you can either sell or buy, but
  not both).  You want to earn as much money as possible.  How?

  NOTE: It's not necessary to make a transaction every day, nor is it
  necessary to hold a share everyday.

  Define:
  fa(i) = We own A at the end of day
  fb(i) = We own B at the end of day
  f0(i) = We own nothing at the end of day
  where each is the maximum paper earnings
  (includes both cash and market value)

  fa(i) = max(f0(i-1), fa(i-1) + pa[i] - pa[i-1])
  fb(i) = max(f0(i-1), fb(i-1) + pb[i] - pb[i-1])
  f0(i) = max(f0(i-1), fa(i-1) + pa[i] - pa[i-1], fb(i-1) + pb[i] - pb[i-1])
  return f0(n)
")

(def n 10000)

(def pa (repeatedly n #(rand-int 100)))
(def pb (repeatedly n #(rand-int 100)))

(defn dp [pa pb]
  (loop [pas (interleave (next pa) pa)
         pbs (interleave (next pb) pb)
         f0 0
         fa 0
         fb 0]
    (if pas
      (let [fa2 (max f0 (+ fa (- (first pas) (second pas))))
            fb2 (max f0 (+ fb (- (first pbs) (second pbs))))
            f02 (max fa2 fb2)]
        (recur (-> pas next next) (-> pbs next next) fa2 fb2 f02))
      f0)))

(println (dp pa pb))
