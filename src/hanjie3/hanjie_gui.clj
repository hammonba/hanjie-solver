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
  (hanjie3.core/apply-cross-step (hanjie3.core/infer-step2 state)))

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
      :size [(* (:width state) (:cell-size state)) (* (:height state) (:cell-size state))]
      ; setup function called only once, during sketch initialization.
      :setup (setup state)
      ; update-state is called on each iteration before draw-state.
      :update update-state
      :draw draw-state
      :features [:keep-on-top]
      :mouse-clicked hanjie-click
      ; This sketch uses functional-mode middleware.
      ; Check quil wiki for more info about middlewares and particularly
      ; fun-mode.
      :middleware [m/fun-mode])))
