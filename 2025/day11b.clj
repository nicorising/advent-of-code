(require '[clojure.string :as str])
(require '[clojure.test :as test])

(def count-paths
  (memoize
   (fn [devices curr visited-dac visited-fft]
     (if (= curr "out")
       (if (and visited-dac visited-fft) 1 0)
       (let [visited-dac (or visited-dac (= curr "dac"))
             visited-fft (or visited-fft (= curr "fft"))]
         (reduce + (map #(count-paths devices % visited-dac visited-fft)
                        (get devices curr))))))))

(defn solve [input]
  (let [devices (apply hash-map
                       (apply concat
                              (map (fn [line]
                                     (let [[from to] (str/split line #": ")]
                                       [from (str/split to #" ")]))
                                   (str/split-lines input))))]
    (count-paths devices "svr" false false)))

(test/deftest day11b
  (test/is (= (solve (slurp "test_data/day11b.txt")) 2)))

(test/run-tests)

(println (solve (slurp "input.txt")))
