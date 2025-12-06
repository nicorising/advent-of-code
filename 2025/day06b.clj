(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn solve [input]
  (let [lines (str/split input #"\n")
        ops (str/split (last lines) #"\s+")
        problems (reduce (fn [acc num]
                           (if (str/blank? num)
                             (conj acc [])
                             (conj (vec (drop-last acc))
                                   (conj (last acc) (Integer/parseInt num)))))
                         [[]]
                         (map (fn [problem]
                                (str/trim (str/join problem)))
                              (apply
                               mapv
                               vector
                               (map vec (drop-last lines)))))]
    (reduce + (map (fn [op args]
                     (apply (resolve (symbol op)) args))
                   ops problems))))

(test/deftest day06b
  (test/is (= (solve (slurp "test_data/day06.txt")) 3263827)))

(test/run-tests)

(println (solve (slurp "input.txt")))
