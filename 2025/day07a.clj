(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn count-splits [beam depth splitters visited]
  (let [next-split (first
                    (first
                     (filter
                      (fn [[_ line]]
                        (some #{beam} line))
                      (map-indexed (fn [idx line]
                                     [idx line])
                                   splitters))))]
    (if next-split
      (let [split-depth (+ depth next-split)]
        (if (some #{[beam split-depth]} visited)
          []
          (let [splitters (drop next-split splitters)
                visited (conj visited [beam split-depth])
                left-splits (count-splits (dec beam) split-depth splitters visited)
                visited (concat visited left-splits)
                right-splits (count-splits (inc beam) split-depth splitters visited)]
            (concat
             [[beam split-depth]]
             left-splits
             right-splits))))
      [])))

(defn solve [input]
  (let [lines (str/split input #"\n")
        beam (str/index-of (first lines) "S")
        splitters
        (map (fn [line]
               (map first
                    (filter second
                            (map-indexed (fn [idx char]
                                           [idx (= char \^)])
                                         line))))
             (rest lines))]
    (count (distinct (count-splits beam 0 splitters [])))))

(test/deftest day07a
  (test/is (= (solve (slurp "test_data/day07.txt")) 21)))

(test/run-tests)

(println (solve (slurp "input.txt")))
