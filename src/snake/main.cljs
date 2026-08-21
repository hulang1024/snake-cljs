(ns snake.main)

; --- core
(def map-width 30)
(def map-height 30)
(def obj-width 12)
(def obj-height 12)
(def obj-colors
  {:snake-head "#ff0000"
   :snake-body "#00ff00"
   :food "#ffff00"})

(def opposite-direction {:up    :down
                         :right :left
                         :down  :up
                         :left  :right})

(def direction-vector {:up    [0 -1]
                       :right [1  0]
                       :down  [0  1]
                       :left  [-1 0]})

(defn rand-point [map-width map-height pred]
  (let [x (rand-int map-width)
        y (rand-int map-height)]
    (if (pred x y)
      [x y]
      (rand-point map-width map-height pred))))

; snake
(defn make-snake [length map-width map-height]
  (let [dir ([:up :right :down :left] (rand-int 4))
        enough-space? (fn [x y]
                        (case dir
                          :up    (< y (- map-height length))
                          :right (> x length)
                          :down  (> y length)
                          :left  (< x (- map-width length))))
        head (rand-point map-width map-height enough-space?)
        opp-dir-vec (direction-vector (opposite-direction dir))
        nodes (vec (for [i (range length)]
                     [(+ (head 0) (* (opp-dir-vec 0) i))
                      (+ (head 1) (* (opp-dir-vec 1) i))]))]
    {:dir dir :nodes nodes}))

(defn move-snake [snake]
  (let [dir (:dir snake)
        dir-vec (direction-vector dir)
        nodes (:nodes snake)
        length (count nodes)
        old-head (first nodes)
        new-head [(+ (old-head 0) (dir-vec 0))
                  (+ (old-head 1) (dir-vec 1))]
        new-body (subvec nodes 0 (- length 1))]
    (assoc snake :nodes (vec (concat [new-head] new-body)))))

(defn grow-snake [snake]
  (let [nodes (:nodes snake)
        tail (last nodes)]
    (assoc snake :nodes (conj nodes tail))))

(defn set-direction [snake dir]
  (if (= (opposite-direction dir) (:dir snake))
    snake
    (assoc snake :dir dir)))

; food
(defn rand-food [map-width map-height snake]
  (rand-point map-width map-height (fn [x y] (not-any? #(= % [x y]) (:nodes snake)))))

; --- draw
(def canvas nil)
(def ctx nil)

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

(defn draw-obj [ctx point type]
  (set! (.-fillStyle ctx) (obj-colors type))
  (.fillRect ctx (* obj-width (point 0)) (* obj-height (point 1)) obj-width obj-height))

(defn draw [game]
  (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
  (let [{:keys [snake food]} game
        nodes (:nodes snake)]
    (draw-obj ctx (first nodes) :snake-head)
    (doseq [node (subvec nodes 1)]
      (draw-obj ctx node :snake-body))
    (when food
      (draw-obj ctx food :food))))

; --- input
(defn keybaord-key->action [key]
  (case key
    "Space" :ok
    "Enter" :ok
    "ArrowUp" :up
    "ArrowRight" :right
    "ArrowDown" :down
    "ArrowLeft" :left
    nil))

(defn set-keyboard-handler [handler]
  (set! (.-onkeydown js/window)
        (fn [event]
          (let [action (keybaord-key->action (.-code event))]
            (when action
              (handler action))))))

(defn init-input [handler]
  (set-keyboard-handler handler))

; --- game
(def interval (atom 0))
(def last-time (atom 0))

(defn start-loop [update-fn]
  (letfn [(loopf [time]
            (let [delta (- time @last-time)]
              (reset! last-time time)
              (update-fn delta)
              (js/requestAnimationFrame loopf)))]
    (loopf 0)))

(def game-state
  (atom {:snake (make-snake 3 map-width map-height)
         :food nil
         :speed 50
         :pause true}))

(defn game-update [delta]
  (when (not (:pause @game-state))
    (when (not (:food @game-state))
      (swap! game-state
             (fn [state]
               (assoc state :food (rand-food map-width map-height (:snake state))))))
    (if (>= @interval (:speed @game-state))
      (do (swap! game-state update :snake move-snake)
          (reset! interval 0))
      (swap! interval + delta))
    (when (= (:food @game-state)
             (first (:nodes (:snake @game-state))))
      (swap! game-state assoc :food nil)
      (swap! game-state update :snake grow-snake)))
  (draw @game-state))

(defn handle-action [action]
  (cond
    (= action :ok) (swap! game-state update :pause not)
    :else (swap! game-state update :snake set-direction action)))

(init-canvas map-width map-height)
(init-input handle-action)
(start-loop game-update)
