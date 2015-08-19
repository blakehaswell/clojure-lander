(ns lander.core
  (:gen-class)
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension)))

;; This is how often the world is updated and rendered. We'll be aiming for 60
;; frames per second (approx 16ms time-step).
(def time-step (/ 1000 60))

;; Accelerations in m/s^2.
(def gravity-acceleration 9.8)
(def thrust-acceleration 20)

;; Rotation speed defined in degrees per second.
(def rotation-speed 90)

(def lander {:x 50 ; x and y coordinates defined in meters.
             :y 50
             :thrust true
             :rotation 20
             :vertical-speed -10
             :horizontal-speed 2.4})

(defn final-speed
  "Calculates a body's final speed (m/s), given an initial speed (m/s)
  and a constant acceleration (m/s^2) which is applied for time (s)."
  [initial-speed acceleration time]
  (+ initial-speed (* acceleration time)))

(defn render [g]
  (doto g
    (.fillRect 0 0 100 100))
  )

(defn create-gui
  []
  (let [panel (doto (proxy [JPanel] []
                      (paintComponent [g] (render g)))
                (.setPreferredSize (Dimension. 640 480)))]
    (doto (JFrame. "Lander")
      (.setContentPane panel)
      (.pack)
      (.setVisible true))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
