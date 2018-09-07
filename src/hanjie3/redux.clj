(ns hanjie3.redux
  "A program to solve hanjies in Clojure"
  (:require [hanjie3.core :as hc]
            [clojure.core.reducers :as r]
            [clojure.tools.logging :as log]
            [clojure.tools.trace :as trace])
  (:import (clojure.lang Keyword PersistentQueue)
           (java.util Arrays)))

(set! *warn-on-reflection* true)


(defrecord PermutationData [filter-fn coll])

(defn progress-permutationdata
  [{:keys [filter-fn coll]} stone-count]
  (->PermutationData (filter-fn stone-count)
                     (conj coll stone-count)))

(defn build-addperm-xform
  "transduce that will add the range from min to max on to the search tree,
   discarding the values that fail the filter function"
  [min-stones max-stones]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result {:keys [coll] :as perm-data}]
       (transduce (comp (map #(progress-permutationdata perm-data %))
                        (filter :filter-fn))
                  rf
                  result
                  (range min-stones (reduce - (inc max-stones) coll)))))))

(defn build-addLast-xform
  "transducer to add remaining stones as the last entry in the search
   PROVIDING THAT THIS PASSES THE FILTER FUNCTION"
  [max-stones]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result {:keys [coll] :as perm-data}]
       (let [perm-data2 (progress-permutationdata perm-data (reduce - max-stones coll))]
         (if (nil? (:filter-fn perm-data2))
           result
           (rf result perm-data2)))))))

(defn build-permute-eduction
  "postpones exploring the next branches in the search tree for as long as possible"
  [min-stones coll max-stones]
  (eduction (build-addperm-xform min-stones max-stones) coll))

(defn multi-permute-eduction
  "returns chain of eductions to crawl the permutation space in as lazy
   as way as possible
   First and last entries have a min size of zero (since blocks map abut the edge of the hanjie)
   Internal entries have a min size of one (since its only a space if it has at least one blank pixel)"
  [filter-fn stones slots]
  (let [first-coll (build-permute-eduction 0
                                           (list (->PermutationData filter-fn []))
                                           stones)
        internal-colls (reduce (partial build-permute-eduction 1)
                               first-coll
                               (repeat (- slots 2) stones))]
    (eduction (build-addLast-xform stones)
              (map :coll)
              internal-colls)))

(def known-off #{:off})
(def known-on #{:on})
(def known-unknown #{:unk})

(defn build-progressive-filter-fn
  "return filter fn the will consume space counts as they become available
  and either return the next filter fn or will return nil.
  Nil means that the filter rejects this space-count"
  [known-pixels [bc & block-counts]]
  (fn [space-count]
    (let [[space-pxs l8-pxs] (split-at space-count known-pixels)]
      (when (and (= space-count (count space-pxs))
                 (not-any? known-on space-pxs))
        (assert (= (nil? bc)) (empty? l8-pxs))
        (let [bc (or bc 0)
              [block-pxs l9-pxs] (split-at bc l8-pxs)]
          (when (and (= bc (count block-pxs))
                     (not-any? known-off block-pxs)
                     (not (known-on (first l9-pxs))))
            (build-progressive-filter-fn l9-pxs block-counts)))))))

(defn constant-progressive-filter-fn
  "just used for repl development. Will never prune the search tree"
  [_]
  constant-progressive-filter-fn)

(defn build-pixelline
  "build a line of pixels out of spaces and blocks.
  This gets called alot, so we use mutable java arrays for performance"
  [length block-counts space-counts]
  (let [^objects pxarr (make-array Keyword length)]
    (loop [idx 0 [^int spc & spcs] space-counts [^int blk & blks] block-counts]
      (Arrays/fill pxarr idx (+ idx spc) :off)
      (Arrays/fill pxarr (+ idx spc) ^int (+ idx spc (or blk 0)) :on)
      (if (nil? spcs)
        pxarr
        (recur (+ idx spc blk) spcs blks)))))

(defn accumulate-uncertainty
  "keeps the pixels that everyone can agree on.
  Any dissent results in :unk"
  ([] nil)
  ([certain] certain)
  ([^objects certain ^objects pixelline]
   (if (nil? certain)
     pixelline
     (let [acc (aclone certain)]
       (doseq [idx (range (alength acc))]
         (when (not= (aget certain idx)
                     (aget pixelline idx))
           (aset acc idx :unk)))
       acc))))

(defn pixel-certainty
  ":unk gets overwritten with a definite value
   definite value with opposing definite value gets a :contradiction
   :contradictions are bad ..."
  [px1 px2]
  (cond
    (= :unk px1) px2
    (= :unk px2) px1
    (= px1 px2) px1
    :else :contradiction))

(defn accumulate-certainty
  "replace :unks with actual values"
  [line1 line2]
  (mapv pixel-certainty line1 line2))

(defn compute-gapcount
  "gapcount is 1 more than blockcount (since gaps may bookend blocks)"
  [{:keys [blocks]}]
  (inc (count blocks)))

(defn compute-gap-pixelcount
  "gap pixelcount is the total number of pixels less those that are known to be consumed by blocks"
  [{:keys [length blocks]}]
  (reduce - length blocks))

(defn infer-knownpixels-for-line
  "crawls the possible gap sizes, accumulating pixels that remain constant for all possibilities."
  [{:keys [length known-pixels blocks gap-count gap-pixelcount]}]
  (let [counter (volatile! 0)
        start-time (System/currentTimeMillis)]
    (transduce (comp (map #(build-pixelline length blocks %))
                     (map #(do (vswap! counter inc) %)))
               (completing accumulate-uncertainty
                           (fn [kp] {:known-pixels (vec kp)
                                     :search-space @counter
                                     :elapsed-millis (- (System/currentTimeMillis) start-time)}))
               (multi-permute-eduction (build-progressive-filter-fn known-pixels blocks)
                                       gap-pixelcount
                                       gap-count))))

(defn update-line
  "recompute the pixels that are known for a line"
  [line]
  (-> line
      (merge (infer-knownpixels-for-line line))
      (update :version (fnil inc 0))))

(defn transpose
  [mtx]
  (apply mapv vector mtx))

(defn cross-compute-for-line
  "add the pixels known from the transpose of the other orientation to ours"
  [our-line other-line]
  (update our-line
          :known-pixels accumulate-certainty other-line))

(defn cross-compute-known-pixels
  "build a matrix of all the pixels that are known by the other orientation
  combine their pixel certainties with our certainties"
  [lines-into lines-from]
  (mapv cross-compute-for-line
        lines-into
        (transpose (mapv :known-pixels lines-from))))

(defn count-pixels
  [pred known-pixels]
  (transduce (filter pred)
             (fn ([c] c)
               ([c _] (inc c)))
             0
             known-pixels))

(defn compute-unknowns-score
  "rough guess at how easy it willl be to calculate better information"
  [{:keys [length known-pixels gap-count gap-pixelcount]}]
  (let [unk-count (count-pixels #{:unk} known-pixels)]
    (if (zero? unk-count)
      -1
      (+ gap-count (- length unk-count)))))

(defn compute-unknowns-comparator
  [h1 h2]
  (compare (compute-unknowns-score h1)
           (compute-unknowns-score h2)))

(defn init-certainty
  "initially everything is unknown"
  [{:keys [length]}]
  (vec (repeat length :unk)))

(defn build-line-details
  [orientation length index blocks]
  (as-> {:length length
         :blocks blocks
         :version 0
         :orientation orientation
         :index index}
        <az->
        (assoc <az-> :known-pixels (init-certainty <az->))
        (assoc <az-> :gap-count (compute-gapcount <az->))
        (assoc <az-> :gap-pixelcount (compute-gap-pixelcount <az->))
        (assoc <az-> :score (compute-unknowns-score <az->))))

(defn init-redux
  [{:keys [down across] :as h}]
  (-> h
      (select-keys [:title :origin])
      (assoc :ranks  (mapv (partial build-line-details :ranks (count down)) (range) across))
      (assoc :files (mapv (partial build-line-details :files (count across)) (range) down))))

(defn build-lagging-transducer
  "creates a transducer that will always run n items behind.
   this is convenient if the pipeline contains futures, which you
   want to start derefing only when a certain number are in flight"
  [n]
  (fn [rf]
    (let [qv (volatile! PersistentQueue/EMPTY)]
      (fn
        ([] (rf))
        ([acc] (reduce rf acc @qv))
        ([acc v]
         (if (< (count @qv) n)
           (do (vswap! qv conj v)
               acc)
           (let [h (peek @qv)]
             (vswap! qv pop)
             (rf acc h))))))))

(def line-ident (juxt :orientation :index))

(defn fork-join-xform
  "transducer that expects incoming thunks, and will get those thunks
   concurrently executed using fork/join"
  [max-tasks-inflight]
  (comp (map r/fjtask)
        (map @#'r/fjfork)
        (build-lagging-transducer max-tasks-inflight)
        (map @#'r/fjjoin)))


(defn parallelize-xform
  [max-tasks-inflight]
  (comp (map future-call)
        (build-lagging-transducer max-tasks-inflight)
        (map deref)))

(defn update-many-lines
  [lines]
  (time (into []
              (comp (map (fn [l] #(update-line l)))
                    (parallelize-xform (count lines)))
              lines)))

(defn sort-lines-by-cmp
  [cmp {:keys [ranks files]}]
  (remove (comp neg? :score) (sort-by cmp (concat ranks files))))

(defn choose-lines-to-process
  [{:keys [ranks files] :as h}]
  (let [ss (sort-lines-by-cmp compute-unknowns-score h)
        #_(comment ss (take (min (quot (count ss) 2) 5) ss))
        ]
    (random-sample 0.5 ss)))

(defn shuffle-lines
  [{:keys [ranks files]}]
  (shuffle (concat ranks files)))

(defn reintegrate-lines
  [h lines]
  (reduce
   (fn [acc {:keys [orientation index] :as l}]
     (assoc-in acc [orientation index] l))
   h
   lines))

(defn build-mtx
  [lines]
  (mapv :known-pixels lines))

(def pprint-lookup
  {:on \O
   :off \space
   :unk \?
   :contradiction \X})

(defn print-mtx
  [mtx]
  (let [sb (StringBuilder.)]
    (doseq [row mtx]
      (doseq [cell row]
        (.append sb (get pprint-lookup cell)))
      (.append sb \newline))
    (str sb)))

(defn spy-hanjie
  [{:keys [files] :as h}]
  (println (print-mtx (build-mtx files)))
  h)


(defn apply-cross-compute
  [{:keys [ranks files] :as h}]
  (let [r2 (cross-compute-known-pixels ranks files)
        f3 (cross-compute-known-pixels files r2)]
    (assoc h :ranks r2 :files f3)))

(defn one-nibble
  [h lines]
  (apply-cross-compute (reintegrate-lines h (update-many-lines lines))))

(defn one-bite
  [h]
  (reduce (comp spy-hanjie one-nibble)
          h
          (partition-all 5 (shuffle-lines h))))

(defn one-step
  "attempts to progess every single line.
   This is not the most efficient strategy (some lines will be more time consuming)
   but it is simple and will not get stuck.
   Seems to be good enough for now too"
  [h]
  (-> h
      (update :ranks update-many-lines)
      apply-cross-compute
      (update :files update-many-lines)
      apply-cross-compute))

(defn rcount
  "count a reducible without having to create a sequence"
  [coll]
  (reduce (fn [c _] (inc c)) 0 coll))

(defn solve
  [n h]
  (loop [n n h h]
    (println n)
    (println (print-mtx (build-mtx (:files h))))
    (if (pos? n)
      (recur (dec n) (time (one-step h)))
      h)))

(defn init-solve
  [n h]
  (solve n (init-redux h)))

(comment (hr/print-mtx (hr/build-mtx (:files (nth (iterate hr/one-step (hr/init-redux hc/peter)) 10)))))

(comment (hr/print-mtx (hr/build-mtx (:files (time (nth (iterate hr/one-step (hr/init-redux hc/ye-eds-special)) 100)))))
         "Elapsed time: 44572.947356 msecs")
