(ns evaluate-hand.card
  (:require [clojure.string :as str]) )

(def ^{:doc "Map of card rank to numeric value, for sorting/comparison."}
  rank-values
  (->> (range 2 11)
       (map (fn [v] [(str v) v]))
       (into {})
       (merge {"Jack"  11
               "Queen" 12
               "King"  13
               "Ace"   14})))

(def ^{:doc "A map of suit names to numeric values, for sorting/comparison."}
  suit-values
  {"Clubs"    0
   "Diamonds" 1
   "Hearts"   2
   "Spades"   3})

(defprotocol Cardable
  (card-rank [c] "Returns standard card rank.")
  (card-suit [c] "Returns standard card suit.")
  (to-card [c] "Converts to a Card.")
  (compare-cards [c1 c2]
    "Compare function for two cards (must be of same type). Analogous to core
     compare function, returning one of #{-1 0 1}."))

(defprotocol CardSortable
  (sort-cards [cards] "Sorts a collection of cards by rank and suit."))

(def ^{:doc "Standard rank values. The internal representation used in Cards."}
  standard-ranks
  #{"2" "3" "4" "5" "6" "7" "8" "9" "10" "Jack" "Queen" "King" "Ace"})

(def ^{:doc "Standard suit values. The internal representation used in Cards."}
  standard-suits
  #{"Clubs" "Diamonds" "Hearts" "Spades"})

(defrecord Card
  [rank suit]
  Cardable
  (card-rank [this] rank)
  (card-suit [this] suit)
  (to-card [this] this)
  (compare-cards [this other]
    (let [rank-compare (compare (get rank-values rank)
                                (get rank-values (card-rank other)))]
      (if-not (zero? rank-compare)
        rank-compare
        (compare (get suit-values suit)
                 (get suit-values (card-suit other)))))))

(defn make-card
  [r s]
  (Card. (card-rank r) (card-suit s)))

(extend-protocol  CardSortable
  clojure.lang.IPersistentCollection
  (sort-cards [this]
    (apply sorted-set-by compare-cards this)))

(defn make-hand
  [cards]
  (set (map to-card cards)))

(def card-input-format
  #"(?i)([2-9]|10|two|three|four|five|six|seven|eight|nine|ten|jack|queen|king|ace|[jqka])(?:\s?of\s)?(clubs|hearts|diamonds|spades|[cdhs])")

(def rank-transforms
  "Map of various rank inputs to standard card ranks."
  {"two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"
   "ten"   "10"
   "j"     "Jack"
   "q"     "Queen"
   "k"     "King"
   "a"     "Ace"})

(def suit-transforms
  {"c" "Clubs"
   "d" "Diamonds"
   "h" "Hearts"
   "s" "Spades"})

(extend-protocol Cardable
  String
  (card-rank [r]
    (if-let [rank (some standard-ranks #{(str/capitalize r)})]
      rank
      (get rank-transforms (str/lower-case r))))
  (card-suit [s]
    (if-let [suit (some standard-suits #{(str/capitalize s)})]
      suit
      (get suit-transforms (str/lower-case s))))
  Long
  (card-rank [r]
    (when (> 11 r 1)
      (str r)))
  clojure.lang.Seqable
  (card-rank [c]
    (card-rank (first c)))
  (card-suit [c]
    (card-suit (first (rest c))))
  (to-card [c]
    (make-card (first c) (first (rest c)))))

(defn standardize-rank
  [r]
  (if (some (into #{} (map str (range 1 11))) #{r})
    r
    (get rank-transforms (str/lower-case r))))

(defn standardize-suit
  [s]
  (if-let [suit (get suit-transforms (str/lower-case s))]
    suit
    (str/capitalize s)))
