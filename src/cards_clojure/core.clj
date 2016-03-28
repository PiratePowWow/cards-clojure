(ns cards-clojure.core
  (:gen-class))

(def suits [:clubs :spades :hearts :diamonds])
(def ranks (range 1 14))
(def rank-names {1 :ace 11 :jack 12 :queen 13 :king})

(defn create-deck []
  (set
    (for [suit suits
          rank ranks]
      {:suit suit
       :rank rank})))

(defn create-hands [deck]
  (set
    (for [c1 deck
          c2 (disj deck c1)
          c3 (disj deck c1 c2)
          c4 (disj deck c1 c2 c3)]
      #{c1 c2 c3 c4})))

(defn flush? [hand]
  (= 1 (count (set (map :suit hand)))))

(defn two-pair? [hand]
  (let [eachRank (frequencies (map :rank hand))
        twoPair? (= '(2 2) (vals eachRank))]
   twoPair?))

(defn three-of-a-kind? [hand]
  (let [eachRank (frequencies (map :rank hand))
        three? (contains? (set (vals eachRank)) 3)]
    three?))

(defn straight? [hand]
  (let [eachRank (vec (sort (set (map :rank hand))))]
    (cond
      (= 4 (count eachRank))
      (cond
        (and (not= (get eachRank 0) 1)
          (= 3 (- (get eachRank 3) (get eachRank 0))))
        true
        (and (= 1 (get eachRank 0))
          (= 3 (- (get eachRank 3) (get eachRank 0))))
        true
        (and (= 1 (get eachRank 0))
          (= 37 (apply + eachRank)))
        true
        :else false))))
        
(defn -main []
  (time
    (let [deck (create-deck)
          hands (create-hands deck)
          flush-hands (filter straight? hands)]
      (count flush-hands))))
