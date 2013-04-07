(comment
  "Given an infinite stream of coin flips uniformly at random,
   generate an infinite stream of dice rolls uniformly at random.
   Suppose you have to pay for each coin flipped.

   One solution is to flip three coins to generate the values 0 through 7.
   Values 0 through 5 represent the dice rolls 1 through 6.  Values 6 and
   7 are thrown away and three more dice are rolled.  Unfortunately, this
   solution is wasteful.

   The solution here is a special case of arithmetic coding.  Imagine the
   unit length is divided into six equal sized lengths representing the six
   possible outcomes of a dice roll.  Each of these lengths are recursively
   divided into six equal lengths and so on, each iteration representing the
   outcome of subsequent dice rolls.  Two variables are maintained
   indicating the range [x, y), and each coin flip divides the range in half.
   Once both end points of the range are within a recursively defined
   interval, a dice roll is determined and can be output.  Under this
   interpretation, the range [x, y) is always getting smaller.

   The implementation here normalizes recursive unit lengths to the interval
   [0, 6).  Initially set x = 0 and y = 6.  A coin flip of 0 gives the range
   [x, m) and a coin flip of 1 gives the range [m, y) where m = (x + y) / 2.
   When x' = floor(x) and y' = ceil(y) are such that y' - x' = 1,
   i.e. x and y are in the same integer range, then the coin flips have
   determined the dice roll, x'.  At this point, the range is scaled by 6
   and we recurse on the range [x'', y'') such that x'' = 6 * (x - x')
   and y'' = 6 * (y - x').

   Although no coin flips are wasted, the number of bits needed to
   accurately represent each end point of the interval grow linearly with
   the number of coin flips consumed.  As such, the space complexity is
   linear with the input and output.

   This program can be run using: clojure dice-from-coins.clj

   The output is a histogram of the dice values rolled.  Each quantity
   should be roughly the same to indicate the uniformity of dice rolls.
   The program takes about 30 seconds to complete on my computer.")

(defn floor
  "The floor of a ratio or an integer.
   The output must fit within an int."
  [x] (int x))

(defn ceil
  "The ceiling of a ratio or an integer."
  [x] (if (ratio? x)
        (let [n  (numerator x)
              d  (denominator x)]
          (quot (+ n d -1) d))
        x))

(defn coin-flips
  "An infinite lazy sequence of random bits."
  [] (lazy-seq (cons (rand-int 2) (coin-flips))))

(defn roll-dice
  "Converts an infinite lazy sequence of coin flips
   into an infinite lazy sequence of dice rolls."
  ([] (roll-dice (coin-flips)))
  ([coins]
     (letfn [(helper [coins x y]
               (if (= 1 (- (ceil y) (floor x)))
                 ;; x and y are in the same window.
                 ;; Output (floor x), then normalize x and y,
                 ;; and recurse on the end of a lazy-seq.
                 (lazy-seq (cons (floor x)
                                 (let [v  (floor x)
                                       x' (* 6 (- x v))
                                       y' (* 6 (- y v))]
                                   (helper coins x' y'))))

                 ;; x and y are different windows
                 (let [m (/ (+ x y) 2)]
                   (if (zero? (first coins))
                     (recur (rest coins) x m)
                     (recur (rest coins) m y)))))]
       (helper coins 0 6))))

(defn histogram
  "Generate pairs [elem count] representing the histogram of a list."
  [xs] (for [[y ys] (group-by identity xs)] [y (count ys)]))

(println (sort-by first (histogram (take 5000 (roll-dice)))))
