(ns snake.time-travel 
  (:require
    [snake.game :as game]))

(defn make-machine []
  {::history []
   ::time 0})

(def max-history (* game/map-width game/map-height))

(defn- append-history [machine game]
  (let [history (conj (::history machine) game)
        history (if (> (count history) max-history)
                  (subvec history 1)
                  history)]
    (assoc machine
           ::history history
           ::time (dec (count history)))))

(defn has-changed? [game0 game1]
  (not= (select-keys game0 [:snake :food])
        (select-keys game1 [:snake :food])))

(defn join [machine game]
  (if (or (empty? (::history machine))
          (has-changed? (peek (::history machine)) game))
    (append-history machine game)
    machine))

(defn backward [machine]
  (let [t (dec (::time machine))]
    (if (>= t 0)
      {:game (nth (::history machine) t)
       :tm (assoc machine ::time t)}
      {:tm machine})))

(defn forward [machine]
  (let [t (inc (::time machine))
        n (count (::history machine))]
    (if (< t n)
      {:game (nth (::history machine) t)
       :tm (assoc machine ::time t)}
      {:tm machine})))

(defn fork [machine]
  (if (seq (::history machine))
    {:game (nth (::history machine) (::time machine))
     :tm (-> (assoc machine
                    ::history (subvec (::history machine) 0 (inc (::time machine))))
             (assoc ::time (dec (count (:history machine)))))}
    {:tm machine}))
