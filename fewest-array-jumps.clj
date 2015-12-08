;; Given an array of positive integers, where each element represents the number
;; of cells you can jump forward, starting at the first index, what's the
;; minimum number of jumps to get to the end?
;;
;; Greedy heuristic is to choose the index between our current position and
;; where we can jump such that the subsequent jump is the furthest.
;; We only need process each cell once, and this gives us O(n) time and O(1)
;; space.

(use '[clojure.test :only (is)])

(defn tails [& xs] (reductions (fn [s _] (rest s)) xs xs))

;; Brute force O(n!) solution.  An empty list produces an empty list.  Otherwise
;; we solve repeatedly for one fewer item as far as we can jump, and select the
;; solution with the fewest jumps.
(defn solve-brute
  ([] '())
  ([x & xs]
     (let [bs (map #(apply solve-brute %1) (apply tails xs))
           ys (map #(map (partial + %1) %2) (iterate inc 1) bs)
           zs (take x ys)
           js (reduce #(if (<= (count %1) (count %2)) %1 %2) zs)]
       (conj js 0))))

;; Another brute force O(n!) solution.  Given the current index and the distance
;; we can jump, compute the solution for each reachable index and choose the
;; solution with the fewest steps.
(defn solve-brute2 [& xs1]
  (let [xs (vec xs1)
        n (count xs)]
    (letfn
        [(helper [i]
           (if (>= i n) '()
               (let [j (+ i (xs i))
                     bs (map helper (range (inc i) (inc j)))
                     cs (map #(conj % i) bs)]
                 (reduce #(if (<= (count %1) (count %2)) %1 %2) cs))))]
    (helper 0))))

;; Jump over the input with `p` as the source jump point, `i` as the first
;; untested index, and `q` as the target jump point (i.e. the index reachable
;; from `p` where the next jump is the furthest).
(defn solve-greedy
  ([] '())
  ([& xs1]
     (let [xs (vec xs1)
           n (count xs)]
       (loop [p 0 i 1 acc '()]
         (let [j (+ p (xs p))]
           (if (or (>= i n) (>= j n))
             (reverse (conj acc p))
             (let [q (reduce #(let [u (+ %1 (xs %1))
                                    v (+ %2 (xs %2))]
                                (if (>= u v) %1 %2))
                             j (range i (inc j)))]
               (recur q (inc j) (conj acc p)))))))))

;; And of course, if you iterate over a list, touching each element only once,
;; you can use `reduce`.  This keeps the current interval [p, q] and the iterval
;; [bi, bj] with the furthest end point that can be jumped to from the current
;; interval.  Whenever we finish the current interval, we "jump" and update
;; the loop values.  The final jump is performed outside the `reduce`.
(defn solve-reduce
  ([] '())
  ([& xs]
     (let [base {:p 0, :q (first xs), :i 0, :bi 0, :bj (first xs), :js '()}
           step (fn [acc x]
                  (let [{:keys [p q i bi bj js]} acc
                        j (+ i x)
                        [ni nj] (if (>= bj j) [bi bj] [i j])]
                    (if (< i q)
                      {:p p, :q q, :i (inc i), :bi ni, :bj nj, :js js}
                      {:p ni, :q nj, :i (inc i), :bi ni, :bj nj, :js (conj js p)})))
           {fp :p fjs :js} (reduce step base xs)]
       (reverse (conj fjs fp)))))

;; Determine if `steps` is a valid solution given `xs`.
(defn valid? [xs1 steps]
  (let [xs (vec xs1)
        n (count xs)
        reachable? (fn [i j] (<= j (+ i (xs i))))]
    (cond
     (zero? n) (empty? steps)
     (= 1 n) (= '(0) steps)
     :else (and
            ;; contained?
            (every? #(and (>= % 0) (< % n)) steps)

            ;; ascending?
            (apply < steps)

            ;; no more steps than input?
            (<= (count steps) n)

            ;; first is zero?
            (zero? (first steps))

            ;; reaches end?
            (let [j (last steps)
                  k (+ j (xs j))]
              (>= k n))
                              
            ;; every step is reachable from last
            (every? true? (map reachable? steps (rest steps)))))))

;; A solution is considered "optimal" if it's both valid and the same number
;; of steps as the brute force solution.
(defn optimal? [& xs]
  (let [brute (apply solve-brute xs)
        brute2 (apply solve-brute2 xs)
        greedy (apply solve-greedy xs)
        reduced (apply solve-reduce xs)]
    (and (valid? xs brute)
         (valid? xs greedy)
         (valid? xs reduced)
         (= (count brute) (count greedy) (count reduced)))))

(defn test-suite [solve]
  (is (empty? (solve)))
  (is (= '(0) (solve 1)))
  (is (= '(0 1) (solve 1 1)))
  (is (= '(0) (solve 2 1)))
  (is (= '(0 1 2) (solve 1 1 1)))
  (is (= '(0) (solve 3 1 1)))
  (is (= '(0) (solve 5 1 7 1 1)))
  (is (= '(0 2) (solve 5 1 7 1 1 1)))
  (is (= '(0 5) (solve 5 1 7 1 1 100 1 1 1 1 1 1)))
  (let [xs '(5 1 7 1 1 1 100 1 1 1 1 1 1)]
    (is (valid? xs (apply solve xs)))))

(test-suite solve-brute)
(test-suite solve-brute2)
(test-suite solve-greedy)
(test-suite solve-reduce)

(println "Passed deterministic unit tests.")

(doseq [i (range 1 10)]
  (print "Randomized test" i "of 10.\r")
  (flush)
  (doseq [n (range 1 20)]
    (let [xs (repeatedly n #(inc (rand-int 10)))]
      (is (apply optimal? xs)))))
(println "Passed randomized unit tests.")