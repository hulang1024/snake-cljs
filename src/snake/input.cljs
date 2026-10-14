(ns snake.input)

(def keyboard-bindings
  {"Enter"      :ok
   "r"          :restart
   "ArrowUp"    :up
   "ArrowRight" :right
   "ArrowDown"  :down
   "ArrowLeft"  :left})

(defn set-keyboard-handler [handler]
  (set! (.-onkeydown js/window)
        (fn [event]
          (let [action (keyboard-bindings (.-key event))]
            (handler (or action (.-key event)))))))
