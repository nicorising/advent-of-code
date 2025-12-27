(require '[clojure.set :as set])
(require '[clojure.string :as str])
(require '[clojure.test :as test])

(defn compress-positions [positions]
  (apply hash-map
         (flatten
          (map-indexed (fn [idx x]
                         [x (inc idx)])
                       (distinct
                        (sort positions))))))

; TODO: Speed up
(defn flood-grid [grid queue]
  (loop [grid grid
         queue queue]
    (if (empty? queue)
      grid
      (let [[x y] (first (vec queue))
            flood (neg? (nth (nth grid x []) y 1))]
        (if (not flood)
          (recur grid (disj queue [x y]))
          (recur
           (assoc grid x (assoc (nth grid x) y 0))
           (set/union (disj queue [x y])
                      #{[(inc x) y]
                        [(dec x) y]
                        [x (inc y)]
                        [x (dec y)]})))))))

(defn build-grid [tiles]
  (let [x-max (inc (apply max (map first tiles)))
        y-max (inc (apply max (map second tiles)))
        grid (vec (repeat (inc x-max) (vec (repeat (inc y-max) -1))))
        pairs (map-indexed (fn [idx tile]
                             [tile (nth tiles (mod (inc idx) (count tiles)))])
                           tiles)
        grid (reduce (fn [grid pair]
                       (let [[[ax ay] [bx by]] pair
                             is-horizontal (not= ax bx)
                             tiles (let [[a b] (if is-horizontal
                                                 [ax bx]
                                                 [ay by])
                                         [a b] (if (< a b)
                                                 [a b]
                                                 [b a])]
                                     (map (fn [pos]
                                            (if is-horizontal
                                              [pos ay]
                                              [ax pos]))
                                          (range a (inc b))))]
                         (reduce (fn [grid [x y]]
                                   (assoc grid x (assoc (nth grid x) y 1)))
                                 grid
                                 tiles)))
                     grid
                     pairs)]
    (flood-grid grid #{[0 0]})))

; To efficiently check whether a pair is valid, we "compress" the provided
; rectlinear polygon, effectively removing unused x/y values. E.g., we turn:
;  7, 1
; 11, 1
; 11, 7
;  9, 7
;  9, 5
;  2, 5
;  2, 3
;  7, 3
; Into:
;  1, 0
;  3, 0
;  3, 3
;  2, 3
;  2, 2
;  0, 2
;  0, 1
;  1, 1
(defn get-valid-pair-checker [tiles]
  (let [x-compressed-map (compress-positions (map first tiles))
        y-compressed-map (compress-positions (map second tiles))
        compressed-map (apply hash-map
                              (apply concat
                                     (map (fn [[x y]]
                                            [[x y] [(get x-compressed-map x)
                                                    (get y-compressed-map y)]])
                                          tiles)))
        grid (build-grid (map #(get compressed-map %1)
                              tiles))]
    (fn [[a b]]
      (let [[a b] [(get compressed-map a) (get compressed-map b)]
            [[ax ay] [bx by]] [a b]
            [ax bx] (if (< ax bx)
                      [ax bx]
                      [bx ax])
            [ay by] (if (< ay by)
                      [ay by]
                      [by ay])]
        (every? #(not= 0 %1)
                (flatten
                 (map (fn [x]
                        (map (fn [y]
                               (nth (nth grid x) y))
                             (range ay (inc by))))
                      (range ax (inc bx)))))))))

(defn solve [input]
  (let [tiles (map (fn [line]
                     (map Integer/parseInt (str/split line #",")))
                   (str/split input #"\n"))
        valid-pair? (get-valid-pair-checker tiles)]
    (apply
     max
     (apply
      concat
      (map-indexed (fn [idx [ax ay]]
                     (map (fn [[bx by]]
                            (if (valid-pair? [[ax ay] [bx by]])
                              (* (inc (abs (- ax bx)))
                                 (inc (abs (- ay by))))
                              0))
                          (drop (inc idx) tiles)))
                   tiles)))))

(test/deftest day09b
  (test/is (= (solve (slurp "test_data/day09.txt")) 24)))

(test/run-tests)

(println (solve (slurp "input.txt")))
