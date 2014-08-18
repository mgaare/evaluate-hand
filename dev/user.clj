(ns user
  (:require [evaluate-hand.core :refer :all]
            [evaluate-hand.card :as card]
            [evaluate-hand.hand :as hand]
            [clojure.pprint :refer (pprint)]
            [clojure.reflect :refer :all]
            [clojure.tools.namespace.repl :refer :all]))

(def test-hand
  (parse-card-input "3s Jc 9d As 9h"))

(def test-hand2 (parse-card-input "5s 10s js as 6s"))

(def test-hand3 (parse-card-input "5s 6d 9c 8h 7c"))
