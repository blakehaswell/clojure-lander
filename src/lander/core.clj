(ns lander.core
  (:gen-class)
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension Rectangle)
           (java.awt.event KeyAdapter KeyEvent)))

;; This is how often the world is updated and rendered. We'll be aiming for 60
;; frames per second (approx 16ms time-step).
(def time-step (/ 1000 60))

;; Accelerations in m/s^2.
(def gravity-acceleration 9.8)
(def thrust-acceleration 20)

;; Rotation speed defined in degrees per second.
(def rotation-speed 90)

(def state (atom {:lander {:x 0 ; x and y coordinates defined in meters.
                           :y 0
                           :thrust false
                           :fuel 100 ; litres
                           :rotation 0
                           :vertical-speed 0
                           :horizontal-speed 0}}))

(defn final-speed
  "Calculates a body's final speed (m/s), given an initial speed (m/s)
  and a constant acceleration (m/s^2) which is applied for time (s)."
  [initial-speed acceleration time]
  (+ initial-speed (* acceleration time)))

(defn apply-gravity
  [state]
  (update-in state
             [:lander :vertical-speed]
             (fn [vertical-speed] (final-speed vertical-speed
                                               (* gravity -1)
                                               (/ time-step 1000)))))

(defn apply-thrust
  [state]
  (if (:thrust (:lander state))
    (update-in state
               [:lander :vertical-speed]
               (fn [vertical-speed]
                 (final-speed vertical-speed
                              thrust-acceleration
                              (/ time-step 1000))))
    state))

(defn update-speed
  [state]
  (-> state
   apply-gravity
   apply-thrust))

(defn update-position
  [state]
  (update-in state
             [:lander :y]
             (fn [y] (- y
                        (* (:vertical-speed (:lander state))
                           (/ time-step 1000))))))

(defn update-state []
  (swap! state (fn [s]
                 (-> s
                     update-speed
                     update-position))))

;; UI ;;

(defn pixels
  "Converts meters to pixels."
  [meters]
  (* meters 10))

(defn render-lander [g lander]
  (doto g
    (.fillRect (pixels (:x lander)) (pixels (:y lander)) 100 100)))

(defn render [g]
  (let [state @state]
    (render-lander g (:lander state))))

(def panel (doto (proxy [JPanel] []
                   (paintComponent [g]
                     (proxy-super paintComponent g)
                     (render g)))
             (.setPreferredSize (Dimension. 640 480))))

(defn thrustKey?
  [event]
  (= (.getKeyCode event) KeyEvent/VK_SPACE))

(defn create-gui
  []
  (doto (JFrame. "Lander")
    (.setContentPane panel)
    (.pack)
    (.setVisible true)
    (.addKeyListener (proxy [KeyAdapter] []
                       (keyPressed [event]
                         (if thrustKey? event)
                           (swap! state assoc-in [:lander :thrust] true))
                       (keyReleased [event]
                         (if (thrustKey? event)
                           (swap! state assoc-in [:lander :thrust] false)))))))

(defn start-loop []
  (let [start-time (System/currentTimeMillis)]
    ;; (println @state)
    (update-state)
    (.repaint panel)
    (Thread/sleep (max 0
                       (- (+ time-step start-time)
                          (System/currentTimeMillis))))
    (recur)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
