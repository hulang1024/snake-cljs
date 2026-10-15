(ns snake.game 
  (:require
    [cljs.math :as math]))

(def map-width 40)
(def map-height 40)

(def ^:private opposite-direction
  {:up    :down
   :right :left
   :down  :up
   :left  :right})

(def ^:private direction-vector
  {:up    [0 -1]
   :right [1  0]
   :down  [0  1]
   :left  [-1 0]})

(def ^:private initial-length 3)

(defn- rand-point [pred]
  (let [x (rand-int map-width)
        y (rand-int map-height)]
    (if (pred x y)
      [x y]
      (rand-point pred))))

(defn- make-snake [length dir]
  (let [dir (or dir ([:up :right :down :left] (rand-int 4)))
        enough-space? (fn [x y]
                        (case dir
                          :up    (< 0 y (- map-height length))
                          :right (< length x (dec map-width))
                          :down  (< length y (dec map-height))
                          :left  (< 0 x (- map-width length))))
        head (rand-point enough-space?)
        opp-dir-vec (direction-vector (opposite-direction dir))
        nodes (vec (for [i (range length)]
                     [(+ (head 0) (* (opp-dir-vec 0) i))
                      (+ (head 1) (* (opp-dir-vec 1) i))]))]
    {:dir dir :nodes nodes}))

(defn- move-snake [snake]
  (let [{:keys [dir nodes]} snake
        dir-vec (direction-vector dir)
        old-head (first nodes)
        new-head (mapv + old-head dir-vec)
        new-body (pop nodes)]
    (assoc snake :nodes (into [new-head] new-body))))

(defn- grow-snake [snake]
  (update snake :nodes #(conj % (peek %))))

(defn- set-direction [snake dir]
  (if (= (opposite-direction dir) (:dir snake))
    snake
    (assoc snake :dir dir)))

(defn- rand-food [snake]
  (rand-point (fn [x y] (not-any? #(= % [x y]) (:nodes snake)))))

(defn- collide [game]
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

(defn level [game]
  (let [length (count (get-in game [:snake :nodes]))]
    (quot (- length initial-length) 10)))

(defn snake-speed [game]
  (let [level (+ 4 (level game))
        seconds (math/pow (- 0.8 (* level 0.007)) level)]
    (/ 1 (* 60 seconds))))

(defn- emit-event [game event]
  (update game :events conj event))

(defn make []
  (let [snake (make-snake initial-length nil)]
    {:snake snake
     :food (rand-food snake)
     :move-acc 0
     :input-queue []
     :events []}))

(defn try-move-snake [game]
  (let [game (if (seq (:input-queue game))
               (-> (update game :snake set-direction (first (:input-queue game)))
                   (assoc :input-queue (subvec (:input-queue game) 1)))
               game)
        moved (update game :snake move-snake)]
    (case (collide moved)
      :food (-> moved
                (assoc :food (rand-food (:snake moved)))
                (assoc :snake (grow-snake (:snake moved))))
      :self (emit-event game :game-over)
      :wall (emit-event game :game-over)
      moved)))

(defn tick [game]
  (let [acc (+ (:move-acc game) (snake-speed game))]
    (loop [acc acc game game]
      (if (>= acc 1)
        (recur (dec acc)
               (try-move-snake game))
        (assoc game :move-acc acc)))))

(defn handle-action [game action]
  (when (and (some #{action} [:up :right :down :left])
             (< (count (:inputs game)) 3))
    (update game :input-queue conj action)))
