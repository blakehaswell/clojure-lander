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
                           :rotate-cw false
                           :rotate-ccw false
                           :fuel 100 ; litres
                           :rotation 0
                           :vertical-speed 0
                           :horizontal-speed 0}}))

(defn final-speed
  "Calculates a body's final speed (m/s), given an initial speed (m/s)
  and a constant acceleration (m/s^2) which is applied for time (s)."
  [initial-speed acceleration time]
  (+ initial-speed (* acceleration time)))

(defn apply-cw-rotation
  [state]
  (if (:rotate-cw (:lander state))
    (update-in state [:lander :rotation]
               (fn [rotation] (mod (+ rotation
                                      (* rotation-speed
                                         (/ time-step 1000)))
                                   360)))
    state))

(defn apply-ccw-rotation
  [state]
  (if (:rotate-ccw (:lander state))
    (update-in state [:lander :rotation]
               (fn [rotation] (mod (- rotation
                                      (* rotation-speed
                                         (/ time-step 1000)))
                                   360)))
    state))

(defn update-rotation
  [state]
  (-> state
      apply-cw-rotation
      apply-ccw-rotation))

(defn apply-gravity
  [state]
  (update-in state
             [:lander :vertical-speed]
             (fn [vertical-speed] (final-speed vertical-speed
                                               (* gravity -1)
                                               (/ time-step 1000)))))

(defn horizontal-thrust-percentage
  [rotation]
  (* -1 (/ (rem (- (rem rotation 360) 180) 180) 90)))

(defn vertical-thrust-percentage
  [rotation]
  (* -1 (/ (rem (- (rem (+ rotation 90) 360) 180) 180) 90)))

(defn apply-thrust
  [state]
  (if (:thrust (:lander state))
    (update-in state [:lander]
               (fn [lander]
                 (-> lander
                     (update-in [:vertical-speed]
                                (fn [vertical-speed]
                                  (final-speed vertical-speed
                                               (* thrust-acceleration
                                                  (vertical-thrust-percentage (:rotation lander)))
                                               (/ time-step 1000))))
                     (update-in [:horizontal-speed]
                                (fn [horizontal-speed]
                                  (final-speed horizontal-speed
                                               (* thrust-acceleration
                                                  (* -1 (horizontal-thrust-percentage (:rotation lander))))
                                               (/ time-step 1000)))))))
    state))

(defn update-speed
  [state]
  (-> state
      apply-gravity
      apply-thrust))

(defn update-position
  [state]
  (update-in state
             [:lander]
             (fn [lander]
               (assoc lander
                      :y (- (:y lander)
                            (* (:vertical-speed lander)
                               (/ time-step 1000)))
                      :x (- (:x lander)
                            (* (:horizontal-speed lander)
                               (/ time-step 1000)))))))

(defn update-state []
  (swap! state (fn [s]
                 (-> s
                     update-rotation
                     update-speed
                     update-position))))

;; UI ;;

(defn pixels
  "Converts meters to pixels."
  [meters]
  (* meters 10))

(defn radians
  "Converts degrees to radians."
  [degrees]
  (* degrees
     (/ Math/PI 180)))

(defn render-lander [g lander]
  (let [x (pixels (:x lander))
        y (pixels (:y lander))]
    (doto g
      (.rotate
       (radians (:rotation lander))
       (+ x 50)
       (+ y 50))
      (.fillRect x y 100 100))))
  

(defn render [g]
  (let [state @state]
    (render-lander g (:lander state))))

(def panel (doto (proxy [JPanel] []
                   (paintComponent [g]
                     (proxy-super paintComponent g)
                     (render g)))
             (.setPreferredSize (Dimension. 640 480))))

(defn thrust-key?
  [event]
  (= (.getKeyCode event) KeyEvent/VK_SPACE))

(defn rotate-cc-key?
  [event]
  (= (.getKeyCode event) KeyEvent/VK_RIGHT))

(defn rotate-ccw-key?
  [event]
  (= (.getKeyCode event) KeyEvent/VK_LEFT))

(defn create-gui
  []
  (doto (JFrame. "Lander")
    (.setContentPane panel)
    (.pack)
    (.setVisible true)
    (.addKeyListener (proxy [KeyAdapter] []
                       (keyPressed [event]
                         (if (thrust-key? event)
                           (swap! state assoc-in [:lander :thrust] true))
                         (if (rotate-cc-key? event)
                           (swap! state assoc-in [:lander :rotate-cw] true))
                         (if (rotate-ccw-key? event)
                           (swap! state assoc-in [:lander :rotate-ccw] true)))
                       (keyReleased [event]
                         (if (thrust-key? event)
                           (swap! state assoc-in [:lander :thrust] false))
                         (if (rotate-cc-key? event)
                           (swap! state assoc-in [:lander :rotate-cw] false))
                         (if (rotate-ccw-key? event)
                           (swap! state assoc-in [:lander :rotate-ccw] false)))))))

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
