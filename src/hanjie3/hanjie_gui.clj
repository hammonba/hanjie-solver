(ns hanjie3.hanjie-gui
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.tools.logging :as log]))

(defn setup
  [state]
  (fn []
    ; Set frame rate to 30 frames per second.
    (q/frame-rate 30)
    ; Set color mode to HSB (HSV) instead of default RGB.
    (q/color-mode :hsb)

    (q/scale 100)
    ; setup function returns initial state. It contains
    ; circle color and position.
    state))

(defn update-state [state]
  (hanjie3.core/apply-cross-step (hanjie3.core/infer-step2 state))
  #_(hanjie3.core/apply-cross-step (hanjie3.core/apply-ratio-step state)))

(defn update-state-ratio [state]
  (hanjie3.core/apply-cross-step (hanjie3.core/apply-ratio-step state)))

(defn rot-update-dir
  [d]
  (condp = d
    :across :down
    :down :across))

(defn update-state-progressive
  [{:keys [update-index update-dir] :as state}]
  (let [[kw-linerule kw-known-px kw-ratio kw-width kw-cross-px]
        (condp = (or update-dir :across)
          :across [:across :known-across :ratio-across :down :known-down]
          :down [:down :known-down :ratio-down :across :known-across])
        update-index (or update-index 0)
        r ((hanjie3.core/infer-line-ratio (count (get state kw-width)))
            (nth (get state kw-known-px) update-index)
            (nth (get state kw-linerule) update-index))
        b (hanjie3.core/ratio2binary-vec r)
        #_(hanjie3.core/soft-combine-pixels
                (hanjie3.core/ratio2binary-vec r)
                (mapv #(nth % update-index) (get state kw-cross-px)))
        end? (= (dec (count (get state kw-known-px))) update-index)
        next-update-index (if end? 0 (inc update-index))
        next-update-dir (if end? (rot-update-dir update-dir) (or update-dir :across))
        ]
    (log/infof "update-index=%s b=%d" b update-index)
    (-> state
        (update kw-known-px assoc update-index b)
        (update kw-ratio assoc update-index r)
        (assoc :update-index next-update-index
               :update-dir next-update-dir)
        hanjie3.core/apply-cross-step))
  )

#_(defn update-state [{:keys [next-task] :as state}]
  (condp = next-task
    :stalled state
    :cross (if-let [s2 (hanjie3.core/apply-cross-step state)]
             (assoc s2 :next-task :infer)
             (assoc state :next-task :stalled))
    (if-let [s2 (hanjie3.core/infer-step2 state)
             #_(hanjie3.core/find-infer-next state)]
      (assoc s2  :next-task :cross)
      (assoc state :next-task :stalled))))

(defn draw-state-ratio [{:keys [width height cell-size ratio-down ratio-across] :as state}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)

  (doseq [x (range width)]
    (q/line 0 (* x cell-size) 0 (* cell-size height)))
  (doseq [y (range height)]
    (q/line 0 (* width cell-size) 0 (* cell-size y)))

  (loop [[r & rs] ratio-down rc 0]
    (let [rcval (* rc cell-size)
          rcnval (+ rcval cell-size)]
      (loop [[c & cs] r cc 0]
        (let [ccval (* cc cell-size)
              ncval (+ ccval cell-size)]
          (q/with-fill [(* (or c 1/2) 255)] (q/triangle ccval rcval
                                               ncval rcval
                                               ccval rcnval)))
        (when cs (recur cs (inc cc)))))
    (when rs (recur rs (inc rc))))

  (loop [[r & rs] ratio-across rc 0]
    (let [rcval (* rc cell-size)
          rcnval (+ rcval cell-size)]
      (loop [[c & cs] r cc 0]
        (let [ccval (* cc cell-size)
              ncval (+ ccval cell-size)]
          (q/with-fill [(* (or c 1/2) 255)] (q/triangle rcnval ncval
                                                        rcval ncval
                                                        rcnval ccval)))
        (when cs (recur cs (inc cc)))))
    (when rs (recur rs (inc rc))))

  ; Set circle color.
  #_(q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  #_(let [angle (:angle state)
          x (* 150 (q/cos angle))
          y (* 150 (q/sin angle))]
      ; Move origin point to the center of the sketch.
      (q/with-translation [(/ (q/width) 2)
                           (/ (q/height) 2)]
                          ; Draw the circle.
                          (q/ellipse x y 100 100)))



  )
(defn draw-state [{:keys [width height cell-size known-down] :as state}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)

  (doseq [x (range width)]
    (q/line 0 (* x cell-size) 0 (* cell-size height)))
  (doseq [y (range height)]
    (q/line 0 (* width cell-size) 0 (* cell-size y)))

  (loop [[r & rs] known-down rc 0]
    (loop [[c & cs] r cc 0]
      (case c
        :on (q/with-fill [0] (q/rect (* cc cell-size) (* rc cell-size) cell-size cell-size))
        :off (q/with-fill [255] (q/rect (* cc cell-size) (* rc cell-size) cell-size cell-size))
        :unk nil
        (do (log/warnf "unexpected: %s " c)
            nil))

      (when cs (recur cs (inc cc))))
    (when rs (recur rs (inc rc))))

  ; Set circle color.
  #_(q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  #_(let [angle (:angle state)
          x (* 150 (q/cos angle))
          y (* 150 (q/sin angle))]
      ; Move origin point to the center of the sketch.
      (q/with-translation [(/ (q/width) 2)
                           (/ (q/height) 2)]
                          ; Draw the circle.
                          (q/ellipse x y 100 100)))



  )

(defn compute-statesize
  [state]
  (assoc state
    :width (count (:known-down state))
    :height (count (first (:known-down state)))
    :cell-size 50))

(defn cell-rot
  [v]
  (case v
    :on :off
    :off :unk
    :unk :on))

(defn hanjie-click
  [state {:keys [x y button] :as click}]
  (let [col (int (/ x (:cell-size state)))
        row (int (/ y (:cell-size state)))]
    (-> state
        (update-in [:known-down row col] cell-rot)
        (update-in [:known-across col row] cell-rot)
        )))

(defn sketch-hanjie
  [state]
  (let [state (compute-statesize state)]
    (q/sketch
      :title "You spin my circle right round"
      :size [(* (:height state) (:cell-size state)) (* (:width state) (:cell-size state))]
      ; setup function called only once, during sketch initialization.
      :setup (setup state)
      ; update-state is called on each iteration before draw-state.
      :update update-state-ratio
      :draw draw-state-ratio
      :features [:keep-on-top]
      :mouse-clicked hanjie-click
      ; This sketch uses functional-mode middleware.
      ; Check quil wiki for more info about middlewares and particularly
      ; fun-mode.
      :middleware [m/fun-mode])))
