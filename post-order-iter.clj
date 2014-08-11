;; This is an implementation of a post order binary tree iterator.
;; The interview question called for a stateful iterator.  Here we
;; provide an `Iterator` protocol and a method `post-order-iter`
;; that returns a stateful instance that reifies the interface.
;; The state maintained is a stack of lambdas indicating the next
;; operation.

(defprotocol Iterator
  (has-more? [this])
  (next-value [this]))

(defn traverse
  ([l x r]
     (do (apply traverse l)
         (apply traverse r)
         (print x)))
  ([x] (print x))
  ([]))

(defn post-order-iter [& xs]
  (let [ops (atom (list))]
    (letfn
        [(push-node
           ([])
           ([x] (swap! ops (partial cons (fn [] x))))
           ([l x r]
              (swap! ops conj
                     (fn [] x)
                     (fn [] (op-node r))
                     (fn [] (op-node l)))))

         (op-node [n] (do (apply push-node n)
                          (next-node)))

         (next-node []
           (let [op (first (deref ops))]
             (swap! ops rest)
             (op)))]

      (dosync (apply push-node xs))
      (reify Iterator
        (has-more? [_] (not (empty? (deref ops))))
        (next-value [_] (next-node))))))

(defn test-post-order-iter []
  (let [tree [[[] 2 [3]] 4 [5]]
        iter (apply post-order-iter tree)]
    (apply traverse tree)
    (println)
    (loop []
      (when (.has-more? iter)
        (print (.next-value iter))
        (recur)))
    (println)))

