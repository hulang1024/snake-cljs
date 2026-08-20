(ns snake.core
  (:require [malli.core :as m]))

; common
; (def Direction [:enum :up :right :down :left])
; (def Point [:int :int])
; (def Snake [:map {:close true}
;             [:dir Direction]
;             [:nodes [#'Point]]])
;
; ; game
; (def Game [:map {:close true}
;            [:snake #'Snake]
;            [:food #'Point]
;            [:speed :int]
;            [:pause :boolean]])

(def ^:private opposite-direction {:up    :down
                                   :right :left
                                   :down  :up
                                   :left  :right})

(def ^:private direction-vector {:up    [0 -1]
                                 :right [1  0]
                                 :down  [0  1]
                                 :left  [-1 0]})

(defn point=? [p x y]
  (and (= x (p 0)) (= y (p 1))))

(defn- rand-point [map-width map-height pred]
  (let [x (rand-int map-width)
        y (rand-int map-height)]
    (if (pred x y)
      [x y]
      (rand-point map-width map-height pred))))

; snake
(defn make-snake [length map-width map-height]
  (let [dir ([:up :right :down :left] (rand-int 4))
        dir-vec (direction-vector (opposite-direction dir))
        enough-space? (fn [x y]
                        (case dir
                          :up    (< y (- map-height length))
                          :right (> x length)
                          :down  (> y length)
                          :left  (< x (- map-width length))))
        head (rand-point map-width map-height enough-space?)
        nodes (vec (for [i (range length)]
                     [(+ (head 0) (* (dir-vec 0) i))
                      (+ (head 1) (* (dir-vec 1) i))]))]
    {:dir dir :nodes nodes}))

(defn move-snake [snake]
  (let [dir (:dir snake)
        dir-vec (direction-vector dir)
        old-nodes (:nodes snake)
        length (count old-nodes)
        old-head (first old-nodes)
        new-head [(+ (old-head 0) (dir-vec 0))
                  (+ (old-head 1) (dir-vec 1))]
        new-body (subvec old-nodes 0 (- length 1))]
    (assoc snake :nodes (vec (concat [new-head] new-body)))))

(defn set-direction [snake dir]
  (if (= (opposite-direction dir) (:dir snake))
    snake
    (assoc snake :dir dir)))

; food
(defn rand-food [map-width map-height snake]
    (rand-point map-width map-height (fn [x y _ _](not-any? #(point=? % x y) (:nodes snake)))))
