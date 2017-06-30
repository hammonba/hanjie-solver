(ns hanjie3.core
  (:require [clojure.core.reducers :as r]
            [clojure.tools.logging :as log]
            [clojure.tools.trace :as trace]
            [clojure.math.combinatorics :as combinatorics])
  (:gen-class))

(defn reduce-count
  [c]
  (reduce (fn [acc _] (inc acc))
          0
          c))

(def other-type
  {:on :off
   :off :on})

(defn is-contradicted?
  [type pxs]
  (when-let [wrong-type (get other-type type)]
    (reduce (fn [_ v]
              (if (= wrong-type v)
                (reduced true)
                false))
            false
            pxs)))

(defn apply-combination-to-bucket
  [init-vec c]
  (reduce #(update %1 %2 inc)
          init-vec
          c))

(defn build-bucket-initvec
  "freeblock starts and ends with zero,
   has 1s inbetween"
  [c]
  (-> (repeat c 1)
      (conj 0)
      (vec)
      (conj 0)))

(defn allocate-buckets
  "allocate spare amongst count buckets"
  [blocks spare known-pixels]
  (let [blocks-count (count blocks)
        init-vec (build-bucket-initvec (dec blocks-count))]
    (eduction
      (map #(mapv + init-vec %))
      (alloc-remaining3 known-pixels 0 blocks spare))))

(defn split-pixels
  "splits the pixels collection into blocksizes"
  [block-sizes pixels]
  (loop [block-sizes block-sizes pixels pixels split-vals '()]
    (if-let [sz (first block-sizes)]
      (recur (rest block-sizes)
             (drop sz pixels)
             (conj split-vals (take sz pixels)))
      split-vals)))

(defn gen-pixels
  "apply the known pixels to the passed gaps.
  * return nil if there is a contradiction
  * return pixels with any additional information inferred"
  [pixels-in blocks]
  (let [ab (map vector (cycle [:off :on]) blocks)]
    (reduce (fn [acc [type block-size pxs]]
              (if (is-contradicted? type pxs)
                (reduced nil)
                (concat acc (repeat block-size type))))
            []
            (map vector
                 (cycle [:off :on])
                 blocks
                 (split-pixels blocks pixels-in)))))

(defn combine-pixel
  "two pixel values the same are kept.
   :unk converts into rhs
   two that are not are marked as :unk"
  [l r]
  (cond
    (= l r) l
    (= :soft-unk l) r
    (= :sort-unk r) l
    :else :unk))

(defn combine-pixels
  "infer knowledge about consistent values"
  [acc-pixels new-pixels]
  (if (some? new-pixels)
    (mapv combine-pixel acc-pixels new-pixels)
    acc-pixels))

(defn soften-unk
  [coll]
  (replace {:unk :soft-unk} coll))

(defn harden-unk
  [coll]
  (replace {:soft-unk :unk} coll))

(defn infer-pixels
  "attempt to infer more pixel information"
  [known-pixels blocks]
  (transduce
    (map #(gen-pixels known-pixels %))
    (completing combine-pixels)
    (soften-unk known-pixels)
    blocks))

(defn gen-combined-blocks
  "eduction of gaps interleaved with blocks"
  [width blocks known-pixels]
  (let [allocated (reduce + (dec (count blocks)) blocks)
        leftover (- width allocated)]
    (eduction
      (map #(interleave % (conj blocks nil)))
      (allocate-buckets blocks leftover known-pixels))))


(defn infer-line
  [width]
  (fn [known-pixels line-rule]
    (infer-pixels known-pixels
                  (gen-combined-blocks width line-rule known-pixels))))

(defn infer-nth-line
  [n width block-rules known-pixels]
  (let [line-pxs (nth known-pixels n)
        line-rules (nth block-rules n)
        line-pxs2 (infer-pixels line-pxs
                                (gen-combined-blocks width line-rules line-pxs))]
    (when (not= line-pxs line-pxs2)
      (assoc known-pixels n line-pxs2))))

(defn find-infer-nth-line
  [width block-rules known-pixels]
  (some #(infer-nth-line % width block-rules known-pixels) (range width)))

(defn find-infer-next
  [{:keys [across known-across down known-down] :as state}]
   (or (when-let [known-across2 (find-infer-nth-line (count down) across known-across)]
         (assoc state :known-across known-across2))
       (when-let [known-down2 (find-infer-nth-line (count across) down known-down)]
         (assoc state :known-down known-down2))))

(defn infer-step
  [width block-rules known-pixels]
  (mapv (infer-line width)
        known-pixels
        block-rules))

(defn par-infer-step
  [width block-rules known-pixels]
  (vec (pmap (infer-line width)
             known-pixels
             block-rules)))

(defn infer-step2
  [{:keys [across known-across down known-down] :as state}]
  (assoc state :known-down (par-infer-step (count known-across) down known-down)
               :known-across (par-infer-step (count known-down) across known-across)))

(defn count-unknowns
  [pixels]
  (reduce
    (fn [c v] (if (= :unk v) v (inc v)))
    0
    pixels))

(defn calc-score
  [w px b]
  (let [pxc (count-unknowns px)]
    [(if (zero? pxc) 99 (- w pxc))
     (- w (count b))]))

(defn heur-infer-step
  [width block-rules known-pixels]
  (let [scores (mapv (fn [idx] (calc-score width (nth known-pixels idx) (nth block-rules idx))) (range width))
        indices (sort-by #(nth scores %) (range width))
        infer-fn (infer-line width)]
    (log/warnf "scores: %s" scores)
    (log/warnf "indices: %s" indices)
    (loop [kpx (transient known-pixels)
           indices indices
           updated 0]
      (cond
        (> updated 3) (persistent! kpx)
        (empty? indices) (persistent! kpx)
        :else (let [idx (first indices)
                    nl (infer-fn (nth known-pixels idx)
                                 (nth block-rules idx))]
                (if (not= nl (nth known-pixels idx))
                  (recur (assoc! kpx idx nl)
                         (rest indices)
                         (inc updated))
                  (recur kpx (rest indices) updated)))))))

(defn apply-infer-step
  [{:keys [known-across across known-down down] :as args}]
  (assoc args
    :known-across (par-infer-step (count down)
                              across
                              known-across)
    :known-down (par-infer-step (count across)
                            down
                            known-down)))

(defn transpose
  [mtx]
  (apply mapv vector mtx))

(defn soft-combine-pixels
  [px1 px2]
  (harden-unk (combine-pixels (soften-unk px1) (soften-unk px2))))

(defn cross-step
  [this that]
  (mapv soft-combine-pixels
        this
        (transpose that)))

(defn apply-cross-step
  [{:keys [known-across known-down] :as args}]
  (assoc args
    :known-across (cross-step known-across known-down)
    :known-down (cross-step known-down known-across)))

(def one-step (comp apply-cross-step apply-infer-step))

(def pprint-lookup
  {:on \O
   :off \space
   :unk \?})

(defn pprint-solution
  [mtx]
  (transduce
    (map #(clojure.string/join (map pprint-lookup %)))
    (completing #(clojure.string/join \newline [%1 %2]))
    ""
    mtx))

(defn solve-hanjie
  [s]
  (pprint-solution (:known-down (ffirst (drop-while #(not= (first %) (second %)) (partition 2 1 (iterate one-step s)))))))

(defn build-init-known
  [r c]
  (vec (repeat (count r) (vec (repeat (count c) :unk)))))

(defn initialise-known
  [{:keys [across down] :as d}]
  (assoc d :known-across (build-init-known down across)
           :known-down (build-init-known across down)))

(def umbrella (initialise-known
                {:title        "umbrella"
                 :origin       "http://www.hanjie.co.uk/hanjie1.php"
                 :across       [[1] [3] [3] [5 4] [5 2] [7 1] [6 2] [14] [6] [7] [5] [5] [3] [3] [1]]
                 :down         [[1] [5] [7] [9] [11] [13] [15] [1 1 1 1 1 1 1] [1] [1] [1] [1 1] [1 1] [2 2] [3]]}))

(def april27 (initialise-known
               {:title  "27 april"
                :origin "http://www.hanjie-star.com/picross/27-april-20118.html"
                :across [[3 1] [2 1] [2 1 1] [2 2 1] [2 2 2 1]
                         [2 8 1] [2 9 1] [3 10 3 1] [13 10] [25]
                         [2 13 2 1] [3 5 3 1] [2 3 3 1] [2 3 2 1] [2 2 1]
                         [2 2 1] [2 1 1] [2 1] [2 1] [1 1]
                         [1 1] [1] [1] [1] [1]]
                :down   [[4 4] [4 4] [3 4] [7] [7]
                         [8] [4 4] [3 4] [2 4] [1 9]
                         [1 13] [15] [7] [3 3] [3 2]
                         [3 3] [5] [4] [4] [4]
                         [2 2] [2 3] [2 2] [2] [25]]}))

(def village (initialise-known
               {:title  "little village under the sky"
                :origin "http://www.hanjie-star.com/picross/little-village-under-the-sky-20312.html"
                :across [[2 1 1 1] [1 2 7 2] [2 1 4 1 1] [2 1 5 1] [1 2 2 1 1]
                         [1 2 2 1 2] [1 2 2 1 1] [1 3 3 1] [2 4 1 2] [2 3 1 2 2]
                         [2 3 1 1 3 1] [2 4 1 1 2 2] [2 2 2 1 1 4] [1 2 2 1 5] [1 3 1 4]
                         [1 3 1 1 2] [2 2 3 1 1 3] [1 2 1 3 4 3] [2 2 2 2 1 2 1 2] [2 1 2 1 1 3]
                         [2 1 2 1 2 2 1] [3 1 1 4 4] [4 2 3 2 1] [3 2 2 2 1] [1 1 1 4]]
                :down   [[9 3 4] [4 5 7] [1 1 2 4] [3 2] [1 6]
                         [2] [2 5 1 3] [15 6] [9 7] [1 4]
                         [4 5] [3 4] [3 4] [1 6] [1]
                         [3] [3 2 5] [1 1 7 9] [1 4 1 1 1 1] [2 5 1 10]
                         [1 1 3 1] [7 6 1] [3 3] [1 1 2 8] [3 13]]}))

(def hedwig (initialise-known
              {:title "hedwig"
               :origin "http://www.hanjie-star.com/picross/hedwig-harry-potter--20515.html"
               :across [[6] [2 1 4 7] [1 1 1 2 2 1] [2 1 1 3 1 1 1] [1 1 1 5 1 1]
                        [1 1 3 1 1] [1 2 1 2 3 3] [1 2 2 1 1] [1 1 1 2 1 2 1 1] [1 1 1 3 3 1]
                        [2 1 1 2 3 1] [1 1 2 2 1 6] [2 3 2 1] [3 1 3 1] [2 3 1]
                        [2 2 2] [2 1 2] [2 2 1] [4] [2]
                        [1] [0] [0] [0] [0]]
               :down [[8] [2 2] [1 1] [1 1] [1 1]
                      [4 3 1] [1 1 1 1] [1 1 1] [1 1 2] [2 6]
                      [2 1 2] [1 1 3 2] [1 1 2 2] [2 1 2 2] [2 2 3 3]
                      [2 5 3] [1 1 1 7] [4 2 3] [10 1] [2 2 4 3]
                      [1 1 1 6] [1 1 1 1] [1 3 1] [1 1 1] [11]]}))
(def inuyasha (initialise-known
                {:title  "inuyasha"
                 :origin "http://www.hanjie-star.com/picross/inuyasha-sesshoumaru-20535.html"
                 :across [[4 2] [2 5 1 4] [1 3 4 2] [2 8 2] [2 11 7]
                          [1 1 1 1 3 2 2] [1 4 2 1 1 5] [2 3 7] [2 3 2 1 7] [2 1 1 1 2 2]
                          [2 1 1 1 1 1] [5 1 2 1 1] [1 2 2 1 1 1 2 2] [1 2 1 2 1 1 2 3] [2 3 1 12 1]
                          [2 9 2 2] [6 1 1 8] [1 2 2 2 1] [1 1 2 9] [2 3 2]
                          [5 5] [1 11 2 1] [5 4 4] [15 2 2] [18 6]]
                 :down   [[3 3 3 3 4] [2 2 1 2 2 3] [1 2 3 2 1 3] [1 1 2 2 1 1 3] [2 1 2 1 1 1 1 3]
                          [1 1 1 2 1 2 1 2 2] [1 1 1 1 2 1 1 1 2] [1 3 2 6 1 2] [2 1 1 1 1 1 1 2] [1 4 5 3 1 2]
                          [3 1 2 1 2 2 1 1 2] [1 3 2 2 1 2] [4 1 4 2 1 2] [2 1 4 1 2] [4 4 1 2]
                          [1 1 3 2 2 1] [1 2 2 1 1 1 1] [1 2 4 1 1 1 1] [2 6 3 1 2] [1 1 4 1 3 1 1 2]
                          [1 5 1 1 7 1] [1 2 2 1 1 5 1 1] [4 2 2 1 1 1 3] [4 3 2 2 1 5] [2 8 2 1 1]]}))

(def peter (initialise-known
             {:title "peter"
              :origin "http://www.hanjie-star.com/picross/peter-20459.html"
              :across [[1] [2] [2] [1 2] [4 3]
                       [8 3] [4 1 5 3] [3 2 2 6 4] [2 1 2 5 2] [1 1 1 1 6]
                       [2 1 8] [1 4 1 3] [1 7 2] [1 1 1] [1 2 1]
                       [1 1 2] [2 1 1 1 2] [2 2 3] [2 2 1] [2 1 1 1]
                       [3 1 2 2] [1 7 2 4] [1 3 2 6] [3 5 5] [1 1 10 4]]
              :down [[8] [3 4] [2 3] [2 2] [2 2 1]
                     [2 1 1 1] [3 4] [2 1 3 4] [3 3 2 2 1] [3 2 1 1 1]
                     [2 1 1 1 1] [2 1 1 1] [3 1 1] [2 1 1] [2 1 1 1 2]
                     [3 3 1 2] [6 4 3] [11 4] [4 1 2] [3 1 1 1]
                     [3 2 2] [4 1 4] [5 2 4] [7 4 5] [8 2 6]]}))

(def snake (initialise-known
             {:title "snake"
              :origin "http://www.hanjie-star.com/picross/snake-20523.html"
              :across [[12 12] [1 1 1 1] [1 10 10 1] [1 1 1 1] [1 1 17 1 1]
                       [1 1 1 1 1 1] [1 1 1 9 3 1 1 1] [1 1 1 1 1 1 1 1 1 1] [1 1 1 1 5 1 1 1 1 1 1] [1 1 1 1 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 3 1 1 1 1] [3 3 1 1 1 1 1 3] [1 1 5 1 1] [5 3 1 1 1 1 3] [1 1 1 3 1 1 1 1 1]
                       [3 1 1 1 3 3 1 1 1 1] [1 1 1 1 1 1 1 1] [1 1 1 13 1 1 1] [1 1 1 1 1 1] [1 1 17 1 1]
                       [1 1 1 1] [1 1 1 1] [1 10 10 1] [1 1 1 1] [12 12]]
              :down [[12 1 10] [1 1 1 1 1] [1 10 1 8 1] [1 1 1 1 1] [1 1 8 7 1 1]
                     [1 1 1 1 1 1 1] [1 1 1 6 5 1 1 1] [1 1 1 1 1 1 1 1 1] [1 1 1 1 6 1 1 1 1 1] [1 1 1 1 1 1 1 1 1]
                     [1 1 1 1 1 6 1 1 1 1] [3 1 1 1 1 1 1 3] [1 1 5 2 1 1] [3 1 1 1 1 1 1 3] [1 1 1 5 1 2 1 1 1 1]
                     [1 1 1 1 1 1 1 1 1 1] [1 1 1 5 4 1 1 1 1] [1 1 1 1 1 1 1 1] [1 1 1 12 1 1 1] [1 1 1 1 1 1]
                     [1 1 16 1 1] [1 1 1 1] [1 10 10 1] [1 1 1 1] [12 12]]
              }))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;(time (solve-hanjie village))
;"Elapsed time: 8376.437485 msecs"
