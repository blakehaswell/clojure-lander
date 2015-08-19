(ns lander.core
  (:gen-class)
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension Rectangle)))

;; This is how often the world is updated and rendered. We'll be aiming for 60
;; frames per second (approx 16ms time-step).
(def time-step (/ 1000 60))

;; Accelerations in m/s^2.
(def gravity-acceleration 9.8)
(def thrust-acceleration 20)

;; Rotation speed defined in degrees per second.
(def rotation-speed 90)

(def state (atom {:lander {:x 50 ; x and y coordinates defined in meters.
                           :y 50
                           :thrust true
                           :fuel 100 ; litres
                           :rotation 20
                           :vertical-speed -10
                           :horizontal-speed 2.4}}))

(defn final-speed
  "Calculates a body's final speed (m/s), given an initial speed (m/s)
  and a constant acceleration (m/s^2) which is applied for time (s)."
  [initial-speed acceleration time]
  (+ initial-speed (* acceleration time)))

(defn update-state []
  (let [current-state @state
        y (:y (:lander current-state))]
    (swap! state assoc-in [:lander :y]
           (+ 1 y))))

(defn render-lander [g lander]
  (doto g
    (.fillRect (:x lander) (:y lander) 100 100)))

(defn render [g]
  (let [state @state]
    (render-lander g (:lander state))))

(def panel (doto (proxy [JPanel] []
                   (paintComponent [g] (render g)))
             (.setPreferredSize (Dimension. 640 480))))

(defn create-gui
  []
  (doto (JFrame. "Lander")
    (.setContentPane panel)
    (.pack)
    (.setVisible true)))

(defn start-loop []
  (let [start-time (System/currentTimeMillis)]
    (update-state)
    (.repaint panel)
    (Thread/sleep (- (+ time-step start-time)
                     (System/currentTimeMillis)))
    (recur)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
