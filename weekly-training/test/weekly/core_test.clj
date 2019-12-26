(ns weekly.core-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]))

(defn fib
  [n]
  (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))
  )

(defn uppercased
  [s]
  (apply str (filter (fn [^Character c] (Character/isUpperCase c)) s))
  )

(defn factorial
  [n]
  (reduce * (range 1 (inc n)))
  )

(defn gcd
  [x y]
  (first
    (filter #(and (= 0 (rem x %)) (= 0 (rem y %)))
            (range (min x y) 0 -1))))

(defn half-truth
  [& bs]
  (true? (and (some false? bs) (some true? bs)))
  )

(defn cartesian
  [c1 c2]
  (set (mapcat (fn [i] (map (fn [j] [i j]) c2)) c1)))

(defn symmetric-diff
  [set1 set2]
  (union (difference set1 set2) (difference set2 set1)))

(defn lcm
  [& ns]
  (if (some #(== 0 %) ns)
    0
    (let [mx (apply max ns)]
      (loop [x mx]
        (if (every? #(= 0 (rem x %)) ns)
          x
          (recur (+ x mx)))))))

(defn pascal
  [n]
  (cond
    (= n 1) [1]
    (= n 2) [1 1]
    :else (let [prev (pascal (dec n))]
            (concat [1]
                    (loop [i 1 acc []]
                      (if (= i (dec n))
                        acc
                        (recur (inc i) (conj acc (+ (nth prev (dec i)) (nth prev i))))
                        ))
                    [1])))
  )

(defn tree?
  [c]
  (if (and (sequential? c) (= 3 (count c)))
    (let [[n [:as l] [:as r]] c]
      (if (and (not (nil? n)) (or (nil? l) (tree? l)) (or (nil? r) (tree? r))) true false)
      )
    false)
  )

;; TODO
(defn symmetric-tree?
  [c]
  (if (and (sequential? c) (= 3 (count c)))
    (let [[n [:as l] [:as r]] c]
      (println (str "c " c " n " n " l " l " r " r))
      (if (and (not (nil? n)) (or (nil? l) (tree? l)) (or (nil? r) (tree? r))) true false)
      )
    false)
  )
(deftest week2
  (testing "exercises"
    ;; week2
    (is (= (fib 8) '(1 1 2 3 5 8 13 21)))
    (is (= (uppercased "$#A(*&987Zf") "AZ"))
    (is (= 6 (some #{2 7 6} [5 6 7 8])))
    (is (= (factorial 8) 40320))
    (is (= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e])))
    (is (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d])))
    (is (= true (half-truth true true true false)))
    (is (= (gcd 1023 858) 33))
    (is (= [1 8 27 64] (map (#(fn [x] (int (Math/pow x %))) 3) [1 2 3 4])))
    (is (= (cartesian #{1 2 3} #{4 5}) #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
    (is (= (symmetric-diff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
    (is (== (lcm 7 5/7 2 3/5) 210))
    (is (= (map pascal (range 1 6)) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
    (is (= (pascal 11) [1 10 45 120 210 252 210 120 45 10 1]))
    ;; week3
    (is (= (tree? '(:a (:b nil nil) nil)) true))
    (is (= (tree? [1 [2 [3 [4 nil nil] nil] nil] nil]) true))
    (is (= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]]) true))
    (is (= (tree? [1 [2 [3 [4 false nil] nil] nil] nil]) false))

    (is (= (symmetric-tree? '(:a (:b nil nil) (:b nil nil))) true))
    (is (= (symmetric-tree?
             [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
           false))
    (is (= (symmetric-tree?
             [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
           true))
    ))
