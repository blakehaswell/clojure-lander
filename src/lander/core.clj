(ns lander.core
  (:gen-class))

;; Accelerations in m/s^2.
(def gravity 9.8)
(def vertical-thrust 20)
(def horizontal-thrust 4)

(defn final-speed
  "Calculates a body's final speed (m/s), given an initial speed (m/s)
  and a constant acceleration (m/s^2) which is applied for time (s)."
  [initial-speed acceleration time]
  (+ initial-speed (* acceleration time)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
