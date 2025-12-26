(require '[clojure.set :as set])
(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn order-boxes [boxes]
  (map second
       (sort
        (apply concat
               (map-indexed
                (fn [idx-a box-a]
                  (map-indexed
                   (fn [offset box-b]
                     (let [idx-b (+ idx-a offset 1)]
                       [(reduce + (map (fn [[a b]]
                                         (* (- a b) (- a b)))
                                       (apply map vector [box-a box-b])))
                        [idx-a idx-b]]))
                   (drop (inc idx-a) boxes)))
                boxes)))))

(defn connect-boxes [connections connect-count circuits]
  (loop [connections connections
         connect-count connect-count
         circuits circuits]
    (if (<= connect-count 0)
      circuits
      (let [[box-a box-b] (first connections)
            get-circuit (fn [box]
                          (first
                           (filter (fn [[_ circuit]]
                                     (contains? circuit box))
                                   (map-indexed (fn [idx circuit]
                                                  [idx circuit])
                                                circuits))))
            [circuit-a-idx circuit-a] (get-circuit box-a)
            [circuit-b-idx circuit-b] (get-circuit box-b)]
        (recur (drop 1 connections)
               (dec connect-count)
               (if (and (nil? circuit-a)
                        (nil? circuit-b))
                 (conj circuits #{box-a box-b})
                 (if (nil? circuit-a)
                   (assoc circuits circuit-b-idx (conj circuit-b box-a))
                   (if (nil? circuit-b)
                     (assoc circuits circuit-a-idx (conj circuit-a box-b))
                     (if (= circuit-a-idx circuit-b-idx)
                       circuits
                       (let [circuits (assoc circuits
                                             circuit-a-idx
                                             (set/union circuit-a circuit-b))]
                         (into (subvec circuits 0 circuit-b-idx)
                               (subvec circuits (inc circuit-b-idx)))))))))))))

(defn solve [input connect-count]
  (let [boxes (map (fn [line]
                     (map Integer/parseInt (str/split line #",")))
                   (str/split input #"\n"))
        connections (order-boxes boxes)]
    (reduce * (take-last 3 (sort (map count (connect-boxes connections
                                                           connect-count
                                                           [])))))))

(test/deftest day08a
  (test/is (= (solve (slurp "test_data/day08.txt") 10) 40)))

(test/run-tests)

(println (solve (slurp "input.txt") 1000))
