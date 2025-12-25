(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn count-splits [beam depth splitters split-counts]
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
        (if (contains? split-counts [beam split-depth])
          [(get split-counts [beam split-depth]) split-counts]
          (let [splitters (drop next-split splitters)
                [left-splits left-split-counts] (count-splits (dec beam)
                                                              split-depth
                                                              splitters
                                                              split-counts)
                split-counts (merge split-counts left-split-counts)
                [right-splits right-split-counts] (count-splits (inc beam)
                                                                split-depth
                                                                splitters
                                                                split-counts)
                split-counts (merge split-counts right-split-counts)
                splits (+ left-splits right-splits)]
            [splits (assoc split-counts [beam split-depth] splits)])))
      [1 split-counts])))

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
    (first (count-splits beam 0 splitters {}))))

(test/deftest day07b
  (test/is (= (solve (slurp "test_data/day07.txt")) 40)))

(test/run-tests)

(println (solve (slurp "input.txt")))
