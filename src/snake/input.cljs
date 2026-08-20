(ns snake.input)

(defn- keybaord-key->action [key]
  (case key
    "Space" :ok
    "Enter" :ok
    "ArrowUp" :up
    "ArrowRight" :right
    "ArrowDown" :down
    "ArrowLeft" :left
    nil))

(defn- set-keyboard-handler [handler]
  (set! (.-onkeydown js/window)
        (fn [event]
          (let [action (keybaord-key->action (.-code event))]
            (when action
              (handler action))))))

(defn init-input [handler]
  (set-keyboard-handler handler))
