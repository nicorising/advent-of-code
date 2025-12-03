(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn solve [input]
  (reduce + (map (fn [line]
                   (let [batteries (map (fn [c] (Character/digit c 10)) line)
                         first-bat (apply max (drop-last batteries))
                         first-bat-idx (.indexOf batteries first-bat)
                         second-bat (apply max (drop (inc first-bat-idx) batteries))]
                     (+ (* first-bat 10) second-bat)))
                 (str/split input #"\n"))))

(test/deftest day03a
  (test/is (= (solve (slurp "test_data/day03.txt")) 357)))

(test/run-tests)

(println (solve (slurp "input.txt")))
