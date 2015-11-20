;; Given an array of positive integers, where each element represents the number
;; of cells you can jump forward, starting at the first index, what's the
;; minimum number of jumps to get to the end?
;;
;; Greedy heuristic is to choose the index between our current position and
;; where we can jump such that the subsequent jump is the furthest.
;; We only need process each cell once, and this gives us O(n) time and O(1)
;; space.

(use '[clojure.test :only (is)])

(defn solve-brute [& xs1]
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

(defn optimal? [& xs]
  (let [brute (apply solve-brute xs)
        greedy (apply solve-greedy xs)]
    (and (valid? xs brute)
         (valid? xs greedy)
         (= (count brute) (count greedy)))))

(defn test-suite [solve]
  (is (empty? (solve)))
  (is (= '(0) (solve 1)))
  (is (= '(0 1) (solve 1 1)))
  (is (= '(0 1 2) (solve 1 1 1)))
  (is (= '(0) (solve 5 1 7 1 1)))
  (is (= '(0 2) (solve 5 1 7 1 1 1)))
  (is (= '(0 5) (solve 5 1 7 1 1 100 1 1 1 1 1 1)))
  (let [xs '(5 1 7 1 1 1 100 1 1 1 1 1 1)]
    (is (valid? xs (apply solve xs)))))

(test-suite solve-brute)
(test-suite solve-greedy)

(println "Passed deterministic unit tests.")
  
(doseq [n (range 1 20)]
  (let [xs (repeatedly n #(inc (rand-int 10)))]
    (is (apply optimal? xs))))

(println "Passed randomized unit tests.")