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

;; Synchronize print operations.
(def printer (agent nil))
(defn println-sync [& args]
  (send printer (fn [_] (apply println args))))

;; First give the customer a shave, then have the customer free up
;; the seat, increment the shave count, and check if the number of
;; shaves is equal to the number of customers.  The reason this
;; is performed by the customer as two separate send operations,
;; is because the second send cannot run until the customer's state
;; is guaranteed to be :shaved.
;; NOTE: In a future remodelling, it might make sense to have the
;; customer's state as a ref instead of an agent, however, the
;; customer still needs an agent so it can request to get its hair cut.
(defn cut-hair [_ cust]
  (do
    (send cust (constantly :shaved))
    (send cust (fn [state]
                 (let [cut-count (dosync
                                  (alter seats-available inc)
                                  (alter hair-cut-count inc)
                                  @hair-cut-count)]
                   (println-sync (str "Customer #" cut-count " got a shave."))
                   (when (= cut-count customer-count)
                     (deliver finished? true))
                   state)))))

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
    (if (every? #(= :shaved (deref %)) customers)
      (println-sync "Everyone is bald.")
      (println-sync "Someone isn't bald!"))
    (await printer)
    (shutdown-agents))

