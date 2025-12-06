(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn solve [input]
  (let [problems (map (fn [problem]
                        [(last problem)
                         (map Integer/parseInt (drop-last problem))])
                      (apply
                       mapv
                       vector
                       (map (fn [line]
                              (str/split (str/trim line) #"\s+"))
                            (str/split input #"\n"))))]
    (reduce (fn [sum [op args]]
              (+ sum
                 (apply (resolve (symbol op)) args)))
            0
            problems)))

(test/deftest day06a
  (test/is (= (solve (slurp "test_data/day06.txt")) 4277556)))

(test/run-tests)

(println (solve (slurp "input.txt")))
