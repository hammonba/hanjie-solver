(ns hanjie3.redux
  "A program to solve hanjies in Clojure"
  (:require [hanjie3.core :as hc]
            [clojure.core.reducers :as r]
            [clojure.tools.logging :as log]
            [clojure.tools.trace :as trace])
  (:import (clojure.lang Keyword PersistentQueue Volatile)
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

(def known-off #(= 0 %))
(def known-on #(= 1 %))

(defn build-progressive-filter-fn
  "return filter fn the will consume space counts as they become available
  and either return the next filter fn or will return nil.
  Nil means that the filter rejects this space-count
  - if any of the proposed spaces are know to be 'on' then reject
  - if any of the subsequent block spaces are known to be 'off' then reject
  - if the pixel immediately following this 'on' block is also 'on' then reject; the block will be too long
  - if we run out of pixels to fulfill the proposed space & block then reject
  - otherwise return a filter function that will consider the next gap/block"
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
  (let [^booleans pxarr (boolean-array length)]
    (loop [idx 0 [^int spc & spcs] space-counts [^int blk & blks] block-counts]
      (Arrays/fill pxarr idx (+ idx spc) false)
      (Arrays/fill pxarr (+ idx spc) ^int (+ idx spc (or blk 0)) true)
      (if (nil? spcs)
        pxarr
        (recur (+ idx spc blk) spcs blks)))))

#_(defn accumulate-uncertainty
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

(defrecord acc-pixel [^int on ^int off ^int unk])
(defrecord PixelAccumulator [^ints on ^Volatile counter])

(defn new-pixel-accumulator [len]
  (->PixelAccumulator (int-array len)
                      (volatile! 0)))
(defn inc-arrelement
  [^ints arr idx]
  (aset ^ints arr idx (inc (aget ^ints arr idx)))
  arr)

(defn accumulate-pixels
  ([pixels] pixels)
  ([{:keys [on ^Volatile counter] :as acc} ^booleans pixelline]
   (doseq [idx (range (alength pixelline))]
          (if (aget pixelline idx)
            (inc-arrelement on idx)))
   (vswap! counter inc)
   acc))

(defn pixel-certainty
  ":unk gets overwritten with a definite value
   definite value with opposing definite value gets a :contradiction
   :contradictions are bad ..."
  [px1 px2]
  (cond
   (= :contradiction px1) :contradiction
   (= :contradiction px2) :contradiction
   (and (= 0 px1) (= 1 px2))  :contradiction
   (and (= 1 px1) (= 0 px2))  :contradiction
   (= 0 px1) 0
   (= 0 px2) 0
   (= 1 px1) 1
   (= 1 px2) 1
   :else (/ (+ px1 px2) 2)))

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
  (let [start-time (System/currentTimeMillis)]
    (transduce (map #(build-pixelline length blocks %))
               (completing accumulate-pixels
                           (fn [kp] (if (zero? @(:counter kp))
                                      nil
                                      {:known-pixels (mapv #(/ % @(:counter kp)) (:on kp))
                                       :elapsed-millis (- (System/currentTimeMillis) start-time)})))
               (new-pixel-accumulator length)
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

(defn count-reducible
  "lazily count the number of reducible values that pass the predicate"
  [pred reduciblel]
  (transduce (filter pred)
             (fn ([c] c)
               ([c _] (inc c)))
             0
             reduciblel))

(defn compute-unknowns-score
  "rough guess at how easy it willl be to calculate better information"
  [{:keys [length known-pixels gap-count gap-pixelcount]}]
  (let [unk-count (count-reducible #{:unk} known-pixels)]
    (if (zero? unk-count)
      -1
      (+ gap-count (- length unk-count)))))

(defn compute-unknowns-comparator
  [h1 h2]
  (compare (compute-unknowns-score h1)
           (compute-unknowns-score h2)))

(defn init-certainty
  "initially everything is unknown; represented by ratio of 1/2"
  [{:keys [length]}]
  (vec (repeat length 1/2)))

(defn build-line-details
  "creates everything that we want to know about a line in order to process it"
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
  "creates our processing data structure from down and across vectors of block"
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
         (vswap! qv conj v)
         (if (< (count @qv) n)
           acc
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
  "creates a transduction of future calls, followed by future derefs
   uses a lagging transducer to control the quantity of un-realised futures
   that may be running simultaneously"
  [max-tasks-inflight]
  (comp (map future-call)
        (build-lagging-transducer 16)
        (map deref)))

(defn update-many-lines
  "applies update-line to all the lines, using clojure futures for simultaneous
  processing"
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
        #_(comment ss (take (min (quot (count ss) 2) 5) ss))]

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
  {1 \O
   0 \space
   :contradiction \X})

(defrecord MinMaxRecord [index orientation minval minidx maxval maxidx])

(defn minmax-pixels-for-line
  "pick out the highest and the lowest value excluding 0 and 1"
      [{:keys [index orientation known-pixels]}]
      (reduce-kv (fn [{:keys [minval minidx maxval maxidx] :as acc} idx v]
                     (cond
                      (= :contradiction v) acc
                      (= 0 v) acc
                      (= 1 v) acc
                      (> v maxval) (assoc acc :maxval v :maxidx idx)
                      (< v maxval) (assoc acc :minval v :minidx idx)
                      :else acc))
                 (->MinMaxRecord index orientation 1/2 -1 1/2 -1)
                 known-pixels))

(defn choose-min-maxes-for-board
  "choose the 5 most likely ons and the 5 most likely offs"
      [{:keys [ranks files]}]
      (let [mms (into (mapv minmax-pixels-for-line ranks)
                      (mapv minmax-pixels-for-line files))]
           {:mins (take 5 (sort-by :minval (filter (comp pos? :minidx) mms)))
            :maxs (take 5 (sort-by :maxval (filter (comp pos? :maxidx) mms)))}))

(defn sortby-closest-to-extremes
  "sort by distance from 0 or 1"
      [{:keys [mins maxs]}]
      (sort-by :distance
               (into (map #(assoc % :distance (:minval %)
                                  :extreme-val 0
                                  :extreme-idx (:minidx %))
                          mins)
                     (map #(assoc % :distance (- 1 (:maxval %))
                                  :extreme-val 1
                                  :extreme-idx (:maxidx %))
                          maxs))))

(defn apply-extremes-guess
      [h {:keys [orientation index extreme-idx extreme-val] :as guess}]
      #_(println `(assoc-in h [~orientation ~index :known-pixels ~extreme-idx] ~extreme-val))
      (when guess
            (assoc-in h [orientation index :known-pixels extreme-idx] extreme-val)))

(defn guess-and-apply
      "make a guess at the most likely uncertain pixel and return board with it"
      [h]
      (apply-extremes-guess h (first (sortby-closest-to-extremes (choose-min-maxes-for-board h)))))

(defn print-mtx
  [mtx]
  (let [sb (StringBuilder.)]
    (doseq [row mtx]
      (doseq [cell row]
        (.append sb (get pprint-lookup cell \?)))
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


(def pprint-hanjie
  (comp println print-mtx build-mtx :files))

(defn pixel-ratio-eqv?
      "two ratios are equivalent neither is zero or one without the other"
      [[r1 r2]]
      (and (= (= 0 r1) (= 0 r2))
           (= (= 1 r1) (= 1 r2))))

(defn stalled-line?
      [[l1 l2]]
      (every? pixel-ratio-eqv? (map vector l1 l2)))

(defn stalled-hanje?
      [h1 h2]
      (when h1
            (every? stalled-line?
                    (map vector
                         (map :known-pixels (:files h1))
                         (map :known-pixels (:files h2))))))

(defn iterate-until-stalled
  "A perfectly formed hanjie will not stall until it is solved.
  A less perfectt hanjie will stall before then
  (and we will have to take a guess at the most likely on/off pixel)"
      [h]
      (loop [prevh nil h h]
            (if (stalled-hanje? prevh h)
              h
              (recur h (one-step h)))))

(defn iterate-then-guess
      [h]
      (loop [h1 h]
            #_(pprint-hanjie h1)

            (let [h2 (iterate-until-stalled h1)
                  h3 (guess-and-apply h2)]
                 (if (nil? h3)
                   h2
                   (recur h3)))))


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

(comment (hr/pprint-hanjie (hr/iterate-then-guess (hr/init-redux ex/peter))))
(comment (hr/pprint-hanjie (hr/iterate-then-guess (hr/init-redux ex/hedwig))))