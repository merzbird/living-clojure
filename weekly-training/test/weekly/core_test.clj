(ns weekly.core-test
  (:require [clojure.test :refer :all]))

(deftest week2
  (testing "exercises"
    (is (= ((fn [n] (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))) 8) '(1 1 2 3 5 8 13 21)))
    (is (= ((fn [s] (apply str (filter (fn [^Character c] (Character/isUpperCase c)) s))) "$#A(*&987Zf") "AZ"))
    (is (= 6 (some #{2 7 6} [5 6 7 8])))
    (is (= ((fn [n] (reduce * (range 1 (inc n)))) 8) 40320))
    (is (= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e])))
    (is (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d])))
    (is (= true ((fn [& bs] (true? (and (some false? bs) (some true? bs)))) true true true false)))
    (is (= ((fn [x y] (first (filter #(and (= 0 (rem x %)) (= 0 (rem y %))) (range (min x y) 0 -1)))) 1023 858) 33))
    (is (= [1 8 27 64] (map (#(fn [x] (int (Math/pow x %))) 3) [1 2 3 4])))
    (is (= ((fn [c1 c2] (set (mapcat (fn [i] (map (fn [j] [i j]) c2)) c1))) #{1 2 3} #{4 5}) #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
    ))
