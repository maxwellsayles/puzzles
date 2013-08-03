(defn add-int-to-string [i s]
  (letfn [(final [c] (if (zero? c) "" (str c)))
          (step [k x]
            (fn [c]
              (let [y  (+ (mod c 10) (- (int x) (int \0)))
                    c' (+ (int (/ c 10)) (int (/ y 10)))
                    x' (mod y 10)]
                (str (k c') x'))))]
    ((reduce step final s) i)))

(println (add-int-to-string 1 "999999999999999"))
(println (add-int-to-string 1 "000000000000000"))
(println (add-int-to-string 10000 "0"))