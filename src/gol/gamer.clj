(ns gol.gamer)

(def h 300)
(def w 300)

;; (set! *warn-on-reflection* true)

;; (def state (make-array Integer/TYPE @w @h))
(def state (atom {}))

(def y- (fn [y] (mod (dec y) h)))
(def y+ (fn [y] (mod (inc y) h)))
(def x- (fn [x] (mod (dec x) w)))
(def x+ (fn [x] (mod (inc x) w)))

(def ones
  {0 0
   1 1
   2 1
   3 2
   4 1
   5 2
   6 2
   7 3})

(def rules
  "
  [1 0 0]
  [0 1 1]  =>  4 3 0
  [0 0 0]

  {430: 1, 37: 0}
  "
  (into {}
    (for [a (range 8)
          b (range 8)
          c (range 8)]
      (let [o (+ (ones a) (ones b) (ones c))]
        [(Integer/parseInt (str a b c))
         (cond
           (= o 3)                      1
           (and (= o 4) (#{2 3 6 7} b)) 1
           :else                        0)]))))



(def sample
  [[1 0 0 1 0 0]
   [0 1 1 0 0 1]
   [0 0 0 0 0 1]
   [0 0 1 1 0 0]
   [0 1 1 1 0 0]
   [0 1 0 0 0 0]])

(def first-step
  [[1 0 0 0 0 0]
   [0 1 1 0 1 1]
   [1 1 0 1 1 0]])


(defn parse [m]
  #_(for [[b c & row] m]
      (loop [x (Integer/parseInt (str (last row) b c) 2)
             out [x]
             [i & in] row]
        (if i
          (let [x' (+ i (bit-shift-left (mod x 4) 1))]
            (recur
              x'
              (conj out x')
              in))
          (conj out (+ b (bit-shift-left (mod x 4) 1))))
        ))

  (let [s (atom {})
        M @m]

    ()
    (doall
      (pmap
        (fn [ys]
          (doseq [y ys]
            (let [i (volatile! (Integer/parseInt (str (get M [(- w 3) y]) (get M [(- w 2) y])) 2))]
              (doseq [x (range w)]
                (let [a  (+ (get M [x y]) @i)
                      i' (bit-shift-left (mod a 4) 1)]
                  (vreset! i i')
                  (swap! s assoc [(x- x) y] a))))))
        (partition-all (quot h 8) (range h))))
    s))


(defn evolve-map! []
  (time
    (let [m @(parse state)]
      (doall
        (pmap
          (fn [xs]
            (doseq [x xs]
              (let [a (volatile! (get m [x (- h 3)]))
                    b (volatile! (get m [x (- h 2)]))]
                (doseq [y (range h)]
                  (let [c (get m [x y])]
                    (swap! state assoc [x (y- y)] (get rules (Integer/parseInt (str @a @b c))))
                    (vreset! a @b)
                    (vreset! b c)))
                )))
          (partition-all (quot w 8) (range w)))))))


(defn random-state! [width height]

  (doseq [x (range w)
          y (range h)]
    (swap! state assoc [x y] (if (> (rand 1) 0.9) 1 0))))

(defn iterate-state [callback]
  (doseq [y (range h)
          x (range w)]
    (when (= 1 (get @state [x y]))
      (callback x y))))


(comment
  [2 4 1 2 4 1
   5 3 6 4 1 2
   4 0 0 0 1 2
   0 1 3 6 5 0
   1 3 7 6 5 0
   1 2 4 0 0 0]

  (random-state! 100 100)

  (evolve-map!)

  (do
    (prn)
    (prn)
    (prn)
    (prn)

    (evolve-map!)
    (doseq [y (range h)]
      (prn (apply str (for [x (range w)]
                        (aget state x y))))))

  (time (parse state))

  (time (evolve-map!))

  )
