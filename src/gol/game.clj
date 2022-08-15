(ns gol.game)

(set! *unchecked-math* true)

(def live-cells (atom {}))

(def h (atom 0))
(def w (atom 0))


(defn random-state! [width height]
  (reset! w width)
  (reset! h height)

  (reset! live-cells
    (reduce (fn [acc [x y]]
              (if (> (rand 1) 0.5)
                (assoc acc [x y] nil)
                acc))
      {}
      (for [x (range @w)
            y (range @h)]
        [x y]))))




(def y- (fn [y] (mod (dec y) @h)))
(def y+ (fn [y] (mod (inc y) @h)))
(def x- (fn [x] (mod (dec x) @w)))
(def x+ (fn [x] (mod (inc x) @w)))


(defn neighbours [x y]
  [[(x- x) (y- y)]
   [(x- x) y]
   [(x- x) (y+ y)]

   [x (y- y)]
   [x (y+ y)]

   [(x+ x) (y- y)]
   [(x+ x) y]
   [(x+ x) (y+ y)]])


(defn blank-neighbours [x y]
  (persistent!
    (reduce
      (fn [acc [x' y']]
        (if (contains? @live-cells [x' y'])
          acc
          (conj! acc [x' y'])))
      (transient [])
      (neighbours x y))))


(defn pm [f]
  (pmap
    f
    (partition-all (quot (count @live-cells) 8) @live-cells)))


(defn evolve-map! []
  #_(let [s (pm
            (fn [chunk]
              ))])
  (time
    (let [s (pmap (fn [items]
                    (persistent!
                      (reduce
                        (fn [acc [[x y]]]
                          (let [ns (blank-neighbours x y)
                                nn (apply concat (for [[x' y'] ns]
                                                   [(when (< 4 (count (blank-neighbours x' y')) 6)
                                                      [x' y'])
                                                    nil]))
                                f  (fn [n] (apply assoc! n nn))]

                            (cond-> acc
                              (< 4 (count ns) 7)
                              (assoc! [x y] nil)

                              (seq nn)
                              f

                              true
                              (dissoc! nil))))

                        (transient {})
                        items)))

              (partition-all (quot (count @live-cells) 8) @live-cells))]

      (reset! live-cells (apply conj s)))))


(defn iterate-state [callback]
  (doseq [[[x y] _] @live-cells]
    (callback x y)))



(comment
  (do (random-state! 600 600) (time (evolve-map!)) nil)

  (do (random-state! 600 600) (time (doseq [[[x y]] @live-cells]
                                      (doseq [k (blank-neighbours x y)]
                                        ))) nil)

  )
