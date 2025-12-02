(require '[clojure.math :as math])
(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn solve [input]
  (first
   (reduce
    (fn [acc x] (let [[zeros pos] acc] ; TODO: Cleanup horrible logic
                  [(+ zeros
                      (abs (int (math/floor (/ (+ (if (and (zero? pos)
                                                           (neg? x))
                                                    100 pos) x) 100))))
                      (if (and (neg? x) (= (mod (+ pos x) 100) 0)) 1 0))
                   (mod (+ pos x) 100)]))
    [0 50]
    (map Integer/parseInt
         (str/split
          (str/replace
           (str/replace input "R" "")
           "L" "-")
          #"\n")))))

(test/deftest day01a
  (test/is (= (solve (slurp "test_data/day01.txt")) 6)))

(test/run-tests)

(println (solve (slurp "input.txt")))
