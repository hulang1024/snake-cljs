(ns snake.main 
  (:require
    [snake.draw :as draw]
    [snake.game :as game]
    [snake.input :as input]
    [snake.time-travel :as tr]))

(def app (atom {:last-time 0
                :accumulator 0
                :update-time 0
                :game (game/make)
                :state :ready
                :tm (tr/make-machine)}))

(defn tick [delta-time _elapsed-ms]
  (when (= (:state @app) :playing)
    (swap! app
           (fn [app]
             (let [game (-> (:game app) (assoc :events []) (game/tick))
                   app (-> (assoc app :game game)
                           (update :tm tr/join game))]
               (if (some #(= % :game-over) (:events game))
                 (assoc app :state :game-over)
                 app)))))
  (draw/draw delta-time @app))

(defn resume-game [app input]
  (let [app (merge app (tr/fork (:tm app)))
        app (if (contains? #{:up :right :down :left} input)
              (update app :game game/handle-action input)
              app)]
    (assoc app :state :playing)))

(defn handle-action [action]
  (swap! app
         (fn [app]
           (if (= action :restart)
             (assoc app :state :playing :game (game/make))
             (case (:state app)
               :ready (resume-game app action)
               :playing (case action
                          :ok (assoc app :state :paused)
                          :time-backward (merge (assoc app :state :paused)
                                                (tr/backward (:tm app)))
                          :time-forward (merge (assoc app :state :paused)
                                               (tr/forward (:tm app)))
                          (update app :game #(game/handle-action % action)))
               :paused (case action 
                         :ok (resume-game app action)
                         :time-backward (merge app (tr/backward (:tm app)))
                         :time-forward (merge app (tr/forward (:tm app)))
                         (resume-game app action))
               :game-over (case action
                            :ok (resume-game app action)
                            :time-backward (merge app (tr/backward (:tm app)))
                            :time-forward (merge app (tr/forward (:tm app)))
                            (resume-game app action))
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
