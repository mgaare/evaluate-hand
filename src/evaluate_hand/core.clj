(ns evaluate-hand.core
  (:require [evaluate-hand.card :as card]
            [evaluate-hand.hand :as hand])
  (:gen-class))

(defn parse-card-input
  "Takes a card input string, and returns either a Hand or nil if
   invalid input."
  [is]
  (let [matches (re-seq card/card-input-format is)]
    (when (= 5 (count matches))
      (card/make-hand (map (partial drop 1) matches)))))

(defn -main
  "Returns best hand value for input hand."
  [& args]
  (if-let [hand (parse-card-input (apply str (interpose " " args)))]
    (println (hand/best-hand-value hand))
    (println "Invalid hand entry. I'm looking for 5 cards, in the form of either"
             "RS where R is Rank and S is Suit e.g. \"5s\" or long-hand e.g."
             "\"five of spades\". In either case, separate the cards with a"
             "space.")))
