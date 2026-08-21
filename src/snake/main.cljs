(ns snake.main)

; --- setting
(def map-width 30)
(def map-height 30)
(def obj-width 12)
(def obj-height 12)
(def obj-colors
  {:snake-head "#ff0000"
   :snake-body "#00ff00"
   :food       "#ffff00"})
(def keyboard-bindings
  {"Enter"      :ok
   "r"          :restart
   "ArrowUp"    :up
   "ArrowRight" :right
   "ArrowDown"  :down
   "ArrowLeft"  :left})

; --- core
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
                          :up    (< 0 y (- map-height length))
                          :right (< length x (dec map-width))
                          :down  (< length y (dec map-height))
                          :left  (< 0 x (- map-width length))))
        head (rand-point map-width map-height enough-space?)
        opp-dir-vec (direction-vector (opposite-direction dir))
        nodes (vec (for [i (range length)]
                     [(+ (head 0) (* (opp-dir-vec 0) i))
                      (+ (head 1) (* (opp-dir-vec 1) i))]))]
    {:dir dir :nodes nodes}))

(defn move-snake [snake]
  (let [{:keys [dir nodes]} snake
        dir-vec (direction-vector dir)
        old-head (first nodes)
        new-head (mapv + old-head dir-vec)
        new-body (pop nodes)]
    (assoc snake :nodes (into [new-head] new-body))))

(defn grow-snake [snake]
  (update snake :nodes #(conj % (peek %))))

(defn set-direction [snake dir]
  (if (= (opposite-direction dir) (:dir snake))
    snake
    (assoc snake :dir dir)))

; food
(defn rand-food [map-width map-height snake]
  (rand-point map-width map-height (fn [x y] (not-any? #(= % [x y]) (:nodes snake)))))

(defn collide [game map-width map-height]
  (let [snake (:snake game)
        nodes (:nodes snake)
        head (first nodes)
        [x y] head
        dir (:dir snake)]
    (cond
      (= head (:food game)) :food
      (some #{head} (rest nodes)) :self
      (case dir
        :up    (< y 0)
        :right (> x (dec map-width))
        :down  (> y (dec map-height))
        :left  (< x 0)) :wall)))

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
(defn set-keyboard-handler [handler]
  (set! (.-onkeydown js/window)
        (fn [event]
          (let [action (keyboard-bindings (.-key event))]
            (when action
              (handler action))))))

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

(defn initial-game-state []
  (let [snake (make-snake 3 map-width map-height)]
    {:snake snake
     :food (rand-food map-width map-height snake)
     :speed 50
     :pause true}))
(def game-state (atom (initial-game-state)))

(defn over-game []
  (reset! game-state (initial-game-state)))

(defn game-update [delta]
  (when (not (:pause @game-state))
    (if (>= @interval (:speed @game-state))
      (do (swap! game-state update :snake move-snake)
          (reset! interval 0))
      (swap! interval + delta))
    (let [collision (collide @game-state map-width map-height)]
      (case collision
        :food (do
                (swap! game-state
                       (fn [state]
                         (assoc state :food (rand-food map-width map-height (:snake state)))))
                (swap! game-state update :snake grow-snake))
        :self (over-game)
        :wall (over-game)
        nil)))
  (draw @game-state))

(defn handle-action [action]
  (case action
    :ok (swap! game-state update :pause not)
    :restart (over-game)
    (when (some #{action} [:up :right :down :left])
      (when (:pause @game-state)
        (swap! game-state update :pause not))
      (swap! game-state update :snake set-direction action))))

(init-canvas map-width map-height)
(set-keyboard-handler handle-action)
(start-loop game-update)
