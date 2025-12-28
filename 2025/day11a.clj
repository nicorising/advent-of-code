(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn count-paths [devices curr]
  (if (= curr "out")
    1
    (reduce + (map #(count-paths devices %)
                   (get devices curr)))))

(defn solve [input]
  (let [devices (apply hash-map
                       (apply concat
                              (map (fn [line]
                                     (let [[from to] (str/split line #": ")]
                                       [from (str/split to #" ")]))
                                   (str/split-lines input))))]
    (count-paths devices "you")))

(test/deftest day11a
  (test/is (= (solve (slurp "test_data/day11a.txt")) 5)))

(test/run-tests)

(println (solve (slurp "input.txt")))
