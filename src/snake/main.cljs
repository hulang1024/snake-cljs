(ns snake.main 
  (:require
    [snake.draw :as draw]
    [snake.game :as game]
    [snake.input :as input]))

(def history-length (* game/map-width game/map-height))

(def app (atom {:last-time 0
                :accumulator 0
                :update-time 0
                :game (game/make)
                :last-game nil
                :history []
                :history-index 0 
                :state :ready}))

(defn- append-history [app]
  (let [history (conj (:history app) (:game app))
        history (if (> (count history) history-length)
                  (subvec history 1)
                  history)]
    (assoc app
           :history history
           :history-index (dec (count history)))))

(defn- changed? [game0 game1]
  (not= (select-keys game0 [:snake :food])
        (select-keys game1 [:snake :food])))

(defn time-backward [app]
  (let [i (dec (:history-index app))]
    (if (>= i 0)
      (assoc app
             :game (nth (:history app) i)
             :history-index i)
      app)))

(defn time-forward [app]
  (let [i (inc (:history-index app))
        n (count (:history app))]
    (if (< i n)
      (assoc app
             :game (nth (:history app) i)
             :history-index i)
      app)))

(defn tick [delta-time _elapsed-ms]
  (when (= (:state @app) :playing)
    (swap! app
           (fn [app]
             (let [game0 (:game app)
                   game (-> game0 (assoc :events []) (game/tick))
                   app (cond-> (assoc app :game game)
                         (changed? game0 game) (append-history))]
               (if (some #(= % :game-over) (:events game))
                 (assoc app
                        :state :game-over
                        :last-game (:game app))
                 app)))))
  (draw/draw delta-time @app))

(defn fork [app]
  (if (seq (:history app))
    (assoc app
           :game (nth (:history app) (:history-index app))
           :history (subvec (:history app) 0 (inc (:history-index app))))
    app))

(defn resume-game [app input]
  (let [app (fork app)
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
                          :time-backward (-> (assoc app :state :paused)
                                             (time-backward))
                          :time-forward (-> (assoc app :state :paused)
                                            (time-forward))
                          (update app :game #(game/handle-action % action)))
               :paused (case action 
                         :ok (resume-game app action)
                         :time-backward (time-backward app)
                         :time-forward (time-forward app)
                         (resume-game app action))
               :game-over (case action
                            :ok (resume-game app action)
                            :time-backward (time-backward app)
                            :time-forward (time-forward app)
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
