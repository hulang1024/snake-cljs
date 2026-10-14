(ns snake.main 
  (:require
    [snake.draw :as draw]
    [snake.game :as game]
    [snake.input :as input]))

(def app (atom {:last-time 0
                :accumulator 0
                :update-time 0
                :game (game/make)
                :state :ready}))

(defn tick [delta-time _elapsed-ms]
  (when (= (:state @app) :playing)
    (swap! app
           (fn [app]
             (let [game (-> (:game app) (assoc :events []) (game/tick))
                   app (assoc app :game game)]
               (if (some #(= % :game-over) (:events game))
                 (assoc app :state :game-over)
                 app)))))
  (draw/draw delta-time @app))

(defn handle-action [action]
  (swap! app
         (fn [app]
           (if (= action :restart)
             (assoc app :game (game/make))
             (case (:state app)
               :ready (assoc app
                             :state :playing
                             :game (game/make))
               :playing (cond
                          (= action :ok) (assoc app :state :paused)

                          (contains? #{:up :right :down :left} action)
                          (update app :game #(game/handle-action % action))

                          :else app)
               :paused (if (= action :ok)
                         (assoc app :state :playing)
                         app)
               :game-over (if (= action :ok)
                            (assoc app
                                   :state :playing
                                   :game (game/make))
                            app)
               app)))))

(defn start-loop [update-fn]
  (letfn [(loopf [now-time]
            (let [{:keys [last-time accumulator update-time]} @app
                  acc (+ accumulator (- now-time last-time))
                  delta-time (/ 1000 60)
                  [acc updated?] (loop [acc acc updated? false]
                                   (if (>= acc delta-time)
                                     (do (update-fn delta-time (- now-time update-time))
                                         (recur (- acc delta-time) true))
                                     [acc updated?]))]
              (swap! app assoc
                     :last-time now-time 
                     :accumulator acc
                     :update-time (if updated? now-time update-time))
              (js/requestAnimationFrame loopf)))]
    (loopf 0)))

; (add-tap js/console.log)
(draw/init-canvas)
(input/set-keyboard-handler handle-action)
(start-loop tick)
