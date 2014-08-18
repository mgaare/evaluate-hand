(ns evaluate-hand.hand
  (:require [evaluate-hand.card :as card]
            [clojure.set :as set])
  (:refer-clojure :exclude [flush]))

;; Stolen shamelessly from clojure contrib
(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))

(defn- nth-group
  [gs n]
  (when (> (count gs) n)
    (-> gs (nth n) rest first)))

(defn- first-group
  [gs]
  (-> gs first rest first))

(defn card-as-text
  [c]
  (str (card/card-rank c) " of " (card/card-suit c)))

(defn hand-as-str
  [hand]
  (->> hand
       (map card-as-text)
       (interpose ", ")
       (apply str)))

(defn remaining-cards
  [hand out]
  (set/difference hand out))

(defn set-of-n
  "Returns either the highest ranking set of n cards (where a pair is n=2,
   three-of-a-kind is n=3, etc) from a hand, or nil if no such set exists."
  [hand n]
  (some->> hand
           (group-by card/card-rank)
           (filter (fn [[k v]] (= n (count v))))
           (sort-by first)
           first-group
           set))

(defn pair
  "Returns either the highest-ranking pair from a hand, or nil."
  [hand]
  (set-of-n hand 2))

(defn three-of-a-kind
  "Returns either a 3-of-a-kind from a hand, or nil."
  [hand]
  (set-of-n hand 3))

(defn four-of-a-kind
  "Returns either a 4-of-a-kind from a hand, or nil."
  [hand]
  (set-of-n hand 4))

(defn flush
  "Returns either the flush from a hand, or nil."
  [hand]
  (let [maybe-flush (->> hand (partition-by card/card-suit))]
    (when (= 1 (count maybe-flush))
      (-> maybe-flush first set))))

(defn straight
  "Returns either the straight from a hand, or nil."
  [hand]
  (let [numeric-rank #(->> % card/card-rank (get card/rank-values))
        sorted-hand (card/sort-cards hand)]
    (loop [h (rest sorted-hand) ms (list (first sorted-hand))]
      (if-not (seq h)
        (when (seq ms)
          (set ms))
        (when (= (numeric-rank (first h))
                 (inc (numeric-rank (first ms))))
          (recur (rest h) (cons (first h) ms)))))))

(defn straight-flush
  [hand]
  (when (and (straight hand) (flush hand))
    hand))

(defn full-house
  "Returns a vector of the three-of-a-kind and the pair, if a full house.
   Else nil."
  [hand]
  (when-let [three (three-of-a-kind hand)]
    (let [rem (set/difference hand three)]
      (when-let [two (pair rem)]
        [three two]))))

(defn two-pair
  "Returns a vector of pair pair, if it's two pair. Else nil."
  [hand]
  (when-let [p (pair hand)]
    (let [rem (set/difference hand p)]
      (when-let [p2 (pair rem)]
        [p p2]))))

(defn royal-flush
  "Returns either the royal flush or nil."
  [hand]
  (when (straight-flush hand)
    (let [top-card (-> hand card/sort-cards reverse first)]
      (when (= "Ace" (card/card-rank top-card))
        hand))))

(defn high-card
  "Returns highest-ranked card."
  [hand]
  (-> hand
      card/sort-cards
      reverse
      first))

(defn best-hand-value
  "Returns string representation of the best hand from the hand."
  [hand]
  (cond-let [h]
    (royal-flush hand) (str "Royal Flush! " (hand-as-str (card/sort-cards h)))
    (straight-flush hand) (str "Straight Flush! "
                               (hand-as-str (card/sort-cards h)))
    (four-of-a-kind hand) (str "Four of a kind! " (hand-as-str h)
                               " - " (hand-as-str (remaining-cards hand h)))
    (full-house hand) (str "Full house! " (card/card-rank (ffirst h)) "s over "
                           (card/card-rank (first (rest h))) "s: "
                           (hand-as-str hand))
    (flush hand) (str "Flush! " (hand-as-str h))
    (straight hand) (str "Straight! " (hand-as-str (card/sort-cards h)))
    (three-of-a-kind hand) (str "Three of a kind. " (hand-as-str h)
                                " - " (remaining-cards hand h))
    (two-pair hand) (str "Two pair. "
                         (card/card-rank (first (first h))) "s and "
                         (card/card-rank (first (rest h))) "s: "
                         (hand-as-str (flatten h)) " - "
                         (hand-as-str (remaining-cards hand h)))
    (pair hand) (str "Pair. " (hand-as-str h)
                     " - " (hand-as-str (remaining-cards hand h)))
    (high-card hand) (str "High card " (card/card-rank h) ": "
                          (hand-as-str hand))))
