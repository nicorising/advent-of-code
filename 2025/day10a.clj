(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn apply-button [lights button]
  (reduce (fn [lights light]
            (assoc lights light (not (nth lights light))))
          lights
          button))

(defn solve-machine [goal-lights buttons queue]
  (loop [queue queue]
    (let [[lights steps] (first queue)
          queue (subvec queue 1)]
      (if (= lights goal-lights)
        steps
        (recur
         (into queue
               (map (fn [button]
                      [(apply-button lights button)
                       (inc steps)])
                    buttons)))))))

(defn solve [input]
  (reduce + (map (fn [line]
                   (let [segments (str/split line #" ")
                         goal-lights (vec
                                      (map #(= %1 \#)
                                           (subs (first segments)
                                                 1
                                                 (dec (count (first segments))))))
                         buttons (vec
                                  (map (fn [button]
                                         (vec
                                          (map Integer/parseInt
                                               (str/split (subs button
                                                                1
                                                                (dec (count button)))
                                                          #","))))
                                       (drop-last (rest segments))))]
                     (solve-machine goal-lights
                                    buttons
                                    [[(vec (repeat (count goal-lights) false)) 0]])))
                 (str/split-lines input))))

(test/deftest day10a
  (test/is (= (solve (slurp "test_data/day10.txt")) 7)))

(test/run-tests)

(println (solve (slurp "input.txt")))
