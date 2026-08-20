(ns snake.main 
  (:require
    [snake.core :refer [make-snake rand-food move-snake set-direction]]
    [snake.draw :refer [draw init-canvas]]))

(def map-width 50)
(def map-height 50)

(def last-time (atom 0))
(def interval (atom 0))

(defn start-loop [update-fn]
  (letfn [(loopf [time]
            (let [delta (- time @last-time)]
              (reset! last-time time)
              (update-fn delta)
              (js/requestAnimationFrame loopf)))]
    (loopf 0)))

(def game-state
  (atom {:snake (make-snake 5 map-width map-height)
         :food nil
         :speed 200
         :pause true}))

(defn frame-update [delta]
  (when (not (:pause @game-state))
    (if (>= @interval (:speed @game-state))
      (do (swap! game-state update :snake move-snake)
          (reset! interval 0))
      (swap! interval + delta))
    (when (not (:food @game-state))
      (swap! game-state update :food rand-food map-width map-height (:snake @game-state))))
  (draw @game-state))

(defn handle-action [action]
  (cond 
    (= action :ok) (swap! game-state update :pause not)
    :else (swap! game-state update :snake set-direction action)))

(defn init-action-handlers [handler]
  (set! (.-onkeydown js/window)
        (fn [event]
          (let [action
                (case event.code
                  "Space" :ok
                  "Enter" :ok
                  "ArrowUp" :up
                  "ArrowRight" :right
                  "ArrowDown" :down
                  "ArrowLeft" :left
                  nil)]
            (when action
              (handler action))))))

(init-canvas map-width map-height)
(init-action-handlers handle-action)
(start-loop frame-update)
