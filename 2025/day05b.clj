(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn attempt-merge [[start-a end-a] [start-b end-b]]
  (if (<= start-b end-a)
    [[start-a (max end-a end-b)]]
    [[start-a end-a] [start-b end-b]]))

(defn solve [input]
  (let [ranges (sort
                (vec
                 (map (fn [range]
                        (vec
                         (map Long/parseLong
                              (str/split range #"-"))))
                      (str/split (first
                                  (str/split input #"\n\n"))
                                 #"\n"))))]
    (reduce (fn [sum [start end]]
              (+ sum (- end start) 1))
            0
            (reduce (fn [acc range]
                      (concat (drop-last acc) (attempt-merge (last acc) range)))
                    (cons (first ranges) nil)
                    (rest ranges)))))

(test/deftest day05b
  (test/is (= (solve (slurp "test_data/day05.txt")) 14)))

(test/run-tests)

(println (solve (slurp "input.txt")))
