(ns snake.draw 
  (:require [clojure.string :as s]
            [goog.style :refer [setStyle]]
            [snake.game :as game]))

(def ^:private app
  (atom {:ctx nil :canvas nil}))

(def ^:private v-width 200)
(def ^:private v-height 200)
(def ^:private obj-size
  (min (/ v-width game/map-width) (/ v-height game/map-height)))

(def ^:private obj-colors
  {:snake-head "#ff5555"
   :snake-body "#55ff55"
   :food       "#ffff55"})

(defn- set-size [canvas-el ctx2d]
  (let [dpr (or (.-devicePixelRatio js/window) 1)
        container-w (- (.-innerWidth js/window) 40)
        container-h (- (.-innerHeight js/window) 40)
        scale (min (/ container-w v-width)
                   (/ container-h v-height))
        canvas-w (* v-width scale)
        canvas-h (* v-height scale)]
    (set! (.-width canvas-el)  (* canvas-w dpr))
    (set! (.-height canvas-el) (* canvas-h dpr))
    (.setTransform ctx2d)
    (.scale ctx2d (* scale dpr) (* scale dpr))
    (setStyle canvas-el #js {:width  (str canvas-w "px")
                             :height (str canvas-h "px")})))

(defn init-canvas []
  (let [canvas-el (js/document.createElement "canvas")
        ctx2d (.getContext canvas-el "2d")]
    (js/document.body.appendChild canvas-el)
    (set-size canvas-el ctx2d)
    (set! (.-imageSmoothingEnabled ctx2d) false)
    (js/window.addEventListener "resize" #(set-size canvas-el ctx2d))
    (swap! app assoc :canvas canvas-el)
    (swap! app assoc :ctx ctx2d)))

(defn- draw-rect [ctx [x y] type]
  (set! (.-fillStyle ctx) (obj-colors type))
  (.fillRect ctx (* obj-size x) (* obj-size y) obj-size obj-size))

(defn- draw-hud [ctx game]
  (set! (.-font ctx) "6px zpix")
  (set! (.-fillStyle ctx) "#fafafa")
  (set! (.-textAlign ctx) "left")
  (set! (.-textBaseline ctx) "top")
  (.fillText ctx (str "LEVEL  " (inc (game/level game))) 2 2)
  (set! (.-textAlign ctx) "center")
  (.fillText ctx (str "SCORE  " (:score game)) (/ v-width 2) 2)
  (set! (.-textAlign ctx) "right")
  (.fillText ctx
             (s/join " "
                     (replace {:left 'L :right 'R :up 'U :down 'D}
                              (:input-queue game)))
             (- v-width 2) 2))

(defn- draw-center-text [ctx text offset-y]
  (set! (.-font ctx) "6px zpix")
  (set! (.-fillStyle ctx) "#fafafa")
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline ctx) "middle")
  (.fillText ctx text (/ v-width 2) (+ (/ v-height 2) (* offset-y 12))))

(defn show-pause-text [ctx]
  (draw-center-text ctx "按 B 或 F 键来前进和后退时间。" -1)
  (draw-center-text ctx "按 Enter 或 Esc 或 方向键从此时间点继续游戏！" 0)
  (draw-center-text ctx "按 R 键重新开始游戏。" 1))

(defn draw [_dt {:keys [game state]}]
  (.clearRect (:ctx @app) 0 0 v-width v-height)
  (let [ctx (:ctx @app)
        {:keys [snake food]} game
        nodes (:nodes snake)]
    (draw-rect ctx (first nodes) :snake-head)
    (doseq [node (subvec nodes 1)]
      (draw-rect ctx node :snake-body))
    (when food
      (draw-rect ctx food :food))
    (draw-hud ctx game)
    (case state
      :ready (draw-center-text ctx "按方向键开始！" 0)
      :paused (show-pause-text ctx)
      :game-over (show-pause-text ctx)
      nil)))
