(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn solve [input]
  (count (filter
          (fn [x] (= (mod x 100) 0))
          (reductions + 50
                      (map Integer/parseInt
                           (str/split
                            (str/replace
                             (str/replace input "R" "")
                             "L" "-")
                            #"\n"))))))

(test/deftest day01a
  (test/is (= (solve (slurp "test_data/day01.txt")) 3)))

(test/run-tests)

(println (solve (slurp "input.txt")))
