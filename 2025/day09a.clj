(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn solve [input]
  (let [tiles (map (fn [line]
                     (map Integer/parseInt (str/split line #",")))
                   (str/split input #"\n"))]
    (apply
     max
     (apply
      concat
      (map-indexed (fn [idx [ax ay]]
                     (map (fn [[bx by]]
                            (* (inc (abs (- ax bx)))
                               (inc (abs (- ay by)))))
                          (drop (inc idx) tiles)))
                   tiles)))))

(test/deftest day09a
  (test/is (= (solve (slurp "test_data/day09.txt")) 50)))

(test/run-tests)

(println (solve (slurp "input.txt")))
