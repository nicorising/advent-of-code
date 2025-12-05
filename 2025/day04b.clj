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

(defn count-removable [grid width height]
  (let [removable (filter (fn [[x y]]
                            (accessable? grid x y))
                          (apply
                           concat
                           (map (fn [x]
                                  (map (fn [y]
                                         [x y])
                                       (range 0 height)))
                                (range 0 width))))]

    (if (empty? removable)
      0
      (+ (count removable)
         (count-removable
          (reduce (fn [grid [x y]]
                    (assoc grid y
                           (assoc (nth grid y) x ".")))
                  grid
                  removable)
          width height)))))

(defn solve [input]
  (let [grid (vec (map (fn [line]
                         (str/split line #""))
                       (str/split input #"\n")))
        height (count grid)
        width (count (first grid))]
    (count-removable grid width height)))

(test/deftest day04b
  (test/is (= (solve (slurp "test_data/day04.txt")) 43)))

(test/run-tests)

(println (solve (slurp "input.txt")))
