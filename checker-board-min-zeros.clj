(comment
  "A checkerboard contains numbers. A checker starts in the
  bottom-left and can only move up or right and ends in the
  top-right.  Take the product of the cells encountered on
  the path and minimize the zeros in the product.

  Since 10 factors into 2 and 5, we count the number of 2
  and 5 factors in each number, and discard the remaining
  factors.  We find the path that minimizes the number of
  2's encountered, and the path that minimizes the number
  of 5's encountered, and then we take the path with the
  minimum of both.  This guarantees that the product will
  have the minimum number of zeros.

  The equation for a given cell is:
  opt[x, y] = value[x, y] + min(opt[x-1, y], opt[x, y-1])
  where opt[x,y] is $infinity$ for invalid indexes.")

(defn board-map [f board]
  "Apply f to each cell on the board."
  (map #(map f %) board))

(defn random-cell []
  "Generate a random cell."
  {:twos (rand-int 3) :fives (rand-int 3)})

(defn random-board [width height]
  "Generate a random board of width * height."
  (repeatedly width #(repeatedly height random-cell)))

(defn min-zeros-dp [board]
  "An O(n) dynamic programming solution to minimizing the zeros."
  (let [reduce-col  (fn [prev-col col]
                      (reductions (fn [z [x y]] (min (+ x y) (+ x z)))
                                  (+ (first col) (first prev-col))
                                  (rest (map vector col prev-col))))
        twos-board  (board-map :twos board)
        fives-board (board-map :fives board)
        twos-first  (reductions + (first twos-board))
        fives-first (reductions + (first fives-board))
        twos-best   (last (reduce reduce-col twos-first (rest twos-board)))
        fives-best  (last (reduce reduce-col fives-first (rest fives-board)))]
    (min twos-best fives-best)))

(defn min-zeros-exp [board]
  "This is an exponential time function to compute the minimum zeros
  in the product of the numbers on the path of a board.

  The helper function below takes a board and the width and height.
  The checker is located on the first element of the first sequence.
  We recurse by reducing the outer and inner sequences and the width and
  height appropriately."
  (letfn [(helper [board w h]
                 (let [v  (first (first board))
                       bw (rest board)       ; same board just w-1
                       bh (map rest board)]  ; same board just h-1
                   (cond (and (= 1 w) (= 1 h)) v
                         (= 1 w) (+ v (helper bh 1 (dec h)))
                         (= 1 h) (+ v (helper bw (dec w) 1))
                         :else (let [x (helper bw (dec w) h)
                                     y (helper bh w (dec h))]
                                 (+ v (min x y))))))]
    (let [w (count board)
          h (count (first board))
          t (board-map :twos board)
          f (board-map :fives board)
          x (helper t w h)
          y (helper f w h)]
      (min x y))))

(let [n 4
      board (random-board n n)]
  (doall (map println board))
  (println)
  (println (min-zeros-dp  board))
  (println (min-zeros-exp board)))

