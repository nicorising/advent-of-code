(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn solve [input]
  (let [[ranges ingredients] (str/split input #"\n\n")
        ranges (map (fn [range]
                      (map Long/parseLong (str/split range #"-")))
                    (str/split ranges #"\n"))
        ingredients (map Long/parseLong (str/split ingredients #"\n"))]
    (count
     (filter (fn [ingredient]
               (some (fn [[start end]]
                       (<= start ingredient end))
                     ranges))
             ingredients))))

(test/deftest day05a
  (test/is (= (solve (slurp "test_data/day05.txt")) 3)))

(test/run-tests)

(println (solve (slurp "input.txt")))
