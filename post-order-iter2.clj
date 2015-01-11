;; Binary tree post order iterator using continuations

(defn post [t k]
  "Takes the binary tree, t, and the continuation, k."
  (letfn [(f ([] (k))
             ([x] [x k])
             ([l x r] (post l (fn [] (post r (fn [] [x k]))))))]
    (apply f t)))

(defn post-order-iter [f t]
  (letfn [(process-iter [i]
            (if (nil? i)
              nil
              (let [[x k] i] (f x) (process-iter (k)))))]
    (process-iter (post t (fn [] nil)))))

(defn to-string [t] (with-out-str (post-order-iter print t)))

(assert (= (to-string []) ""))
(assert (= (to-string [1]) "1"))
(assert (= (to-string [[1] 2 []]) "12"))
(assert (= (to-string [[] 2 [3]]) "32"))
(assert (= (to-string [[1] 2 [3]]) "132"))
(assert (= (to-string [[[1] 2 [3]] 4 [[5] 6 [7]]]) "1325764"))
(println "All tests passed.")
