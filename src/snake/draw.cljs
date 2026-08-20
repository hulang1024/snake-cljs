(ns snake.draw)

(def ^:private canvas nil)
(def ^:private ctx nil)
(def ^:private obj-width 15)
(def ^:private obj-height 15)

(defn init-canvas [map-width map-height]
  (let [width (* obj-width map-width)
        height (* obj-height map-height)]
    (set! canvas (js/document.createElement "canvas"))
    (set! ctx (.getContext canvas "2d"))
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (set! (.-width (.-style canvas)) width)
    (set! (.-height (.-style canvas)) height)
    (.appendChild js/document.body canvas)))

(defn- draw-obj [ctx point type]
  (let [color (case type
                :snake-head "#ff0000"
                :snake-body "#00ff00"
                :food "#ffff00")]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx (* obj-width (point 0)) (* obj-height (point 1)) obj-width obj-height)))

(defn draw [game]
  (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
  (let [{:keys [snake food]} game
        nodes (:nodes snake)]
    (draw-obj ctx (first nodes) :snake-head)
    (doseq [node (subvec nodes 1)]
      (draw-obj ctx node :snake-body))
    (when food
      (draw-obj ctx food :food))))

