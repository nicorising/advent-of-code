(require '[clojure.math :as math])
(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn best-batteries [batteries remaining]
  (if (neg? remaining)
    '()
    (let [battery (apply max (drop-last remaining batteries))
          batteries-left (drop (inc (.indexOf batteries battery)) batteries)]
      (cons battery (best-batteries batteries-left (dec remaining))))))

(defn solve [input]
  (reduce +
          (map (fn [line]
                 (let [batteries (map (fn [c] (Character/digit c 10)) line)]
                   (first
                    (reduce (fn [acc curr]
                              (let [[sum idx] acc]
                                [(+ sum (* curr (long (math/pow 10 idx))))
                                 (inc idx)]))
                            [0 0]
                            (reverse (best-batteries batteries 11))))))
               (str/split input #"\n"))))

(test/deftest day03b
  (test/is (= (solve (slurp "test_data/day03.txt")) 3121910778619)))

(test/run-tests)

(println (solve (slurp "input.txt")))
