(ns snake.main
  (:require [snake.core :refer [make-snake move-snake grow-snake rand-food set-direction point=?]]
            [snake.draw :refer [draw init-canvas]]
            [snake.input :refer [init-input]]))

(def map-width 30)
(def map-height 30)

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
    (when (point=? (:food @game-state)
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
