(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn accessable? [grid x y]
  (if (not= (nth (nth grid y) x) "@")
    false
    (<=
     (count
      (filter (fn [cell]
                (= cell "@"))
              (flatten
               (map (fn [neighbor-x]
                      (map (fn [neighbor-y]
                             (nth (nth grid neighbor-y []) neighbor-x "."))
                           (range (dec y) (+ y 2))))
                    (range (dec x) (+ x 2))))))
     4)))

(defn solve [input]
  (count
   (let [grid (map (fn [line]
                     (str/split line #""))
                   (str/split input #"\n"))
         height (count grid)
         width (count (first grid))]
     (filter true?
             (flatten
              (map (fn [x]
                     (map (fn [y]
                            (accessable? grid x y))
                          (range 0 height)))
                   (range 0 width)))))))

(test/deftest day04a
  (test/is (= (solve (slurp "test_data/day04.txt")) 13)))

(test/run-tests)

(println (solve (slurp "input.txt")))
