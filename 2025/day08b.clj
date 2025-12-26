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

(defn last-connection [connections circuits last-connect total-boxes]
  (loop [connections connections
         circuits circuits
         last-connect last-connect]
    (if (and (= (count circuits) 1)
             (= (count (first circuits)) total-boxes))
      last-connect
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
                               (subvec circuits (inc circuit-b-idx))))))))
               [box-a box-b])))))

(defn solve [input]
  (let [boxes (map (fn [line]
                     (map Integer/parseInt (str/split line #",")))
                   (str/split input #"\n"))
        connections (order-boxes boxes)
        [last-a last-b] (last-connection connections [] nil (count boxes))]
    (* (first (nth boxes last-a))
       (first (nth boxes last-b)))))

(test/deftest day08b
  (test/is (= (solve (slurp "test_data/day08.txt")) 25272)))

(test/run-tests)

(println (solve (slurp "input.txt")))
