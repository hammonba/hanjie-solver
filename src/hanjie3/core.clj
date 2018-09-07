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

#_(defn alloc-remaining
   [buckets beans]
   (if (zero? buckets)
     (list (list beans))
     (mapcat
       (fn [alloc-now]
         (map conj (alloc-remaining (dec buckets) (- beans alloc-now))
              (repeat alloc-now)))
       (range (inc beans)))))

#_(defn alloc-remaining2
   [buckets beans]
   (if (zero? buckets)
     (list (list beans))
     (eduction
       (mapcat
         (fn [alloc-now]
           (map conj
                (alloc-remaining2 (dec buckets)
                                  (- beans alloc-now))
                (repeat alloc-now))))
       (range (inc beans)))))

(defn alloc-remaining3
  [known-pixels offset block-sizes to-alloc]
  (if (empty? block-sizes)
    (list (list to-alloc))
    (eduction
      (mapcat
        (fn [alloc-here]
          (let [gap-start offset
                gap-end (let [ge (+ gap-start alloc-here)]
                          (if (zero? offset)
                            ge
                            (+ ge 1)))
                block-start gap-end
                block-end (+ block-start (first block-sizes))]
            (when-not (or (is-contradicted?
                            :off
                            (subvec known-pixels gap-start gap-end))
                          (is-contradicted?
                            :on
                            (subvec known-pixels block-start block-end)))
              (map conj
                   (alloc-remaining3 known-pixels
                                     block-end
                                     (rest block-sizes)
                                     (- to-alloc alloc-here))
                   (repeat alloc-here))))))
      (range (inc to-alloc)))))

#_(defn apply-alloc-to-bucket
   [init-vec alloc]
   (eduction
     (map #(mapv + init-vec %))
     alloc))

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

(def pixelkw-tonum
  {:on 1
   :off 0
   :unk 1/2})

(defn combine-pixels-ratio
  ([[count px-scores]]
   (mapv #(if (zero? %)
            0
            (/ % count)) px-scores))
  ([[count px-scores :as acc] new-pxs]
   [(inc count) (mapv +' px-scores (map pixelkw-tonum new-pxs))]))

(defn soften-unk
  [coll]
  (replace {:unk :soft-unk} coll))

(defn harden-unk
  [coll]
  (replace {:soft-unk :unk} coll))

(defn infer-pixels-ratio
  "attempt to infer more pixel information"
  [width known-pixels blocks]
  (transduce
    (keep #(gen-pixels known-pixels %))
    combine-pixels-ratio
    [0 (vec (repeat width 0))]
    blocks))

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

(defn fj-infer-line
  [width known-pixels line-rule]
  (reduce
   (fn [cp g]
     (combine-pixels cp (@#'r/fjjoin g)))
   (soften-unk known-pixels)
   (seque
    (map (fn [b]
           (@#'r/fjfork (r/fjtask (fn [] (gen-pixels known-pixels b)))))
         (gen-combined-blocks width line-rule known-pixels)))))

  
(defn infer-line
  [width]
  (fn [known-pixels line-rule]
    (infer-pixels known-pixels
                  (gen-combined-blocks width line-rule known-pixels))))

(defn infer-line-ratio
  [width]
  (fn [known-pixels line-rule]
    (infer-pixels-ratio width
                        known-pixels
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

(defn infer-step-ratio
  [width block-rules known-pixels]
  (mapv (infer-line-ratio width)
        known-pixels
        block-rules))

(defn par-infer-step
  [width block-rules known-pixels]
  (vec (pmap (infer-line width)
             known-pixels
             block-rules)))

(defn fj-infer-step
  [width block-rules known-pixels]
  (into []
        (map @#'r/fjjoin)
        (map (fn [k b] (@#'r/fjfork
                        (r/fjtask (fn [] (fj-infer-line width k b)))))
             known-pixels
             block-rules)))

#_(def p5 (nth (iterate one-step peter) 4))
#_((infer-line-ratio (count (:down p5)))
   (nth (:known-across p5) 23)
   (nth (:across p5) 23))

#_(infer-step-ratio (count (:down p5))
                (:across p5)
                (:known-across p5))

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
    :known-across (infer-step (count down)
                              across
                              known-across)
    :known-down (infer-step (count across)
                            down
                            known-down)))

(defn fj-apply-infer-step
  [{:keys [known-across across known-down down] :as args}]
  (assoc args
    :known-across (fj-infer-step (count down)
                              across
                              known-across)
    :known-down (fj-infer-step (count across)
                            down
                            known-down)))

(defn ratio2binary
  [r]
  (condp = r
    0 :off
    1 :on
    :unk))

(defn ratio2binary-vec
  [rv]
  (mapv ratio2binary rv))

(defn ratio2binary-mtx
  [rm]
  (mapv ratio2binary-vec rm))

(defn apply-ratio-step
  [{:keys [known-across across known-down down] :as args}]
  (let [ratio-across (infer-step-ratio (count down)
                                       across
                                       known-across)
        ratio-down (infer-step-ratio (count across)
                                     down
                                     known-down)]
    (assoc args
      :ratio-across ratio-across
      :known-across (ratio2binary-mtx ratio-across)
      :ratio-down ratio-down
      :known-down (ratio2binary-mtx ratio-down))))

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

(def one-step (comp apply-cross-step fj-apply-infer-step))
(def one-step-ratio (comp apply-cross-step apply-ratio-step))
(defn isfinished? [[p q]]
  (= (:ratio-across p) (:ratio-across q)))

(def pprint-lookup
  {:on \O
   :off \space
   :unk \?
   :contradiction \X})

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

(defn solve-hanjie-ratio
  [s]
  (pprint-solution (:known-down (ffirst (drop-while (complement isfinished?)
                                                    (partition 2 1
                                                               (iterate one-step-ratio s)))))))

(defn build-init-known
  [v r c]
  (vec (repeat (count r) (vec (repeat (count c) v)))))

(defn initialise-known
  [{:keys [across down] :as d}]
  (assoc d :known-across (build-init-known :unk across down)
           :ratio-across (build-init-known 1/2 across down)
           :known-down (build-init-known :unk down across)
           :ratio-down (build-init-known 1/2 down across)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;(time (solve-hanjie village))
;"Elapsed time: 8376.437485 msecs"
