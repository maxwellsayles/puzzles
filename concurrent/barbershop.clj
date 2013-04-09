;; This is my Clojuresque solution to the sleeping barber problem
;; (http://en.wikipedia.org/wiki/Sleeping_barber_problem).
;;
;; We create one agent per customer, and one agent for the barber.
;; We ask each customer to try to take a seat.  If the customer
;; was able to take a seat, the customer then asks the barber for a
;; shave.  If the customer was not able to take a seat, the customer
;; asks himself to try again later. The barber, when asked to shave
;; a customer, makes the seat available, shaves the customer, and
;; increments the number of shaves he's performed. Once the barber
;; has performed a number of shaves equal to the total number of
;; customers, he signals that we are finished.
;;
;; This was a bit tricky to eliminate race conditions despite having
;; STM and agents available.  Ultimately, each customer is an agent
;; where the agent value is a reference to the state of the customer.
;; The original implementation had the state of the agent as the state
;; of the cusomter (rather than as a reference).  The problem with this
;; was that the agent state is only set *after* a call to (send) returns.
;; Since this is done asynchronously, the only way to know when a (send)
;; finishes, is to place another (send) on the same agent (since an
;; agent will process sends synchronously and in order if they originate
;; from the same calling agent).  This solution worked, but had the customer
;; essentially doing the work of shaving himself.
;;
;; The solution here adds a layer of indirection to the customer's state.
;; Now the barber can shave a customer in a transaction and then increment
;; the number of shaves in the same transaction.  That way there is no race
;; condition.  Yay!
(def total-seats 4)
(def customer-count 1000)
(def customers (doall (repeatedly customer-count #(agent (ref :hairy)))))
(def barber (agent nil))
(def seats-available (ref total-seats))
(def hair-cut-count (ref 0))
(def finished? (promise))

;; Synchronize print operations.
(def printer (agent nil))
(defn println-sync [& args]
  (send printer (fn [_] (apply println args))))

;; First make a seat available, then shave the customer, and increment
;; the hair cut count.  If the cut count is equal to the total customer
;; count, signal that we are finished.
(defn cut-hair [_ cust-ref]
  (let [cut-count (dosync
                   (alter seats-available inc)
                   (ref-set cust-ref :shaved)
                   (alter hair-cut-count inc)
                   @hair-cut-count)]
    (println-sync (str "Customer #" cut-count " got a shave."))
    (when (= cut-count customer-count)
      (deliver finished? true))))

;; If there's a seat available, have the customer take
;; a seat, and then ask the barber to shave him.
;; Otherwise, ask the customer to try again later.
(defn take-seat [cust-ref cust-agent]
  (let [seated (dosync
                (when (pos? @seats-available)
                  (alter seats-available dec)
                  true))]
    (if seated
      (send barber cut-hair cust-ref)
      (send cust-agent take-seat cust-agent))
    cust-ref))

;; Have each customer take a seat,
;; then wait for everyone to get a shave,
;; and finally verify that all the customers have been shaved.
(do (doseq [cust customers] (send cust take-seat cust))
    @finished?
    (if (every? (fn [cust] (= :shaved @@cust)) customers)
      (println-sync "Everyone is bald.")
      (println-sync "Someone isn't bald!"))
    (await printer)
    (shutdown-agents))

