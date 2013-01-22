;; This is my Clojuresque solution to the sleeping barber problem
;; (http://en.wikipedia.org/wiki/Sleeping_barber_problem).
;;
;; We create one agent per customer, and one agent for the barber.
;; We ask each customer to try to take a seat.  If the customer
;; was able to take a seat, the customer then asks the barber for a
;; shave.  If the customer was not able to take a seat, the customer
;; asks himself to try again later. The barber, when asked to shave
;; a customer, makes the seat available, increments the number of shaves
;; he's performed, and then tells the customer that he has been shaved.
;; Once the barber has shaved 'customer-count' number of customers, the
;; main program then checks that each customer's state is ':shaved'.
(def total-seats 4)
(def customer-count 1000)
(def customers (doall (repeatedly customer-count #(agent :hairy))))
(def barber (agent nil))
(def seats-available (ref total-seats))
(def hair-cut-count (ref 0))
(def finished? (promise))

;; Give the customer a shave, make his seat available,
;; and put another notch in our belt.  If we have one
;; notch for every customer, then deliver true to 'finished?'.
(defn cut-hair [_ cust]
  (let [cut-count (dosync
                   (println (str "Cutting hair #" @hair-cut-count))
                   (alter hair-cut-count inc)
                   (alter seats-available inc)
                   @hair-cut-count)]
    (send cust (fn [_] :shaved))
    (when (= cut-count customer-count)
      (deliver finished? true))))
    
;; If there's a seat available, have the customer take
;; a seat, and then ask the barber to shave him.
;; Otherwise, ask the customer to try again later.
(defn take-seat [state cust]
  (let [seated (dosync
                (when (pos? @seats-available)
                  (alter seats-available dec)
                  true))]
    (if seated
      (send barber cut-hair cust)
      (send cust take-seat cust))
    state))

;; Have each customer take a seat,
;; then wait for everyone to get a shave,
;; and finally verify that all the customers have been shaved.
(do (doseq [cust customers] (send cust take-seat cust))
    @finished?
    (if (and (map #(= :shaved (deref %)) customers))
      (println "Everyone is bald.")
      (println "Someone isn't bald!"))
    (shutdown-agents))

