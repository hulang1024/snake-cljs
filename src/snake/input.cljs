(ns snake.input)

(def keyboard-bindings
  {"Enter"      :ok
   "Escape"     :ok
   "r"          :restart
   "b"          :time-backward
   "f"          :time-forward
   "w"          :up
   "d"          :right
   "s"          :down
   "a"          :left
   "ArrowUp"    :up
   "ArrowRight" :right
   "ArrowDown"  :down
   "ArrowLeft"  :left})

(defn set-keyboard-handler [handler]
  (set! (.-onkeydown js/window)
        (fn [event]
          (when-let [action (keyboard-bindings (.-key event))]
            (handler action)))))
