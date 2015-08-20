(ns lander.core
  (:gen-class)
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension Rectangle)
           (java.awt.event KeyAdapter KeyEvent)))

;; This is how often the world is updated and rendered. We'll be aiming for 60
;; frames per second (approx 16ms time-step).
(def time-step (/ 1000 60))

;; Accelerations in m/s^2.
(def gravity-acceleration -1.6)
(def thrust-acceleration 16)

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

(defn update-state []
  (swap! state (fn [s]
                 (let [lander (:lander s)
                       x (:x lander)
                       y (:y lander)
                       q (cond (>= (:rotation lander) 270) 4
                               (>= (:rotation lander) 180) 3
                               (>= (:rotation lander) 90) 2
                               :else 1)
                       xdir (if (or (= q 1)
                                    (= q 2))
                              1
                              -1)
                       ydir (if (or (= q 1)
                                    (= q 4))
                              1
                              -1)
                       r (Math/toRadians
                          (let [mr (mod (:rotation lander) 90)]
                            (if (or (= q 2)
                                    (= q 4))
                              (- 90 mr)
                              mr)))
                       a (if (:thrust lander)
                           thrust-acceleration
                           0)
                       dt (/ time-step 1000)
                       ax (* a
                             (Math/sin r)
                             xdir)
                       ay (+ (* a
                                (Math/cos r)
                                ydir)
                             gravity-acceleration)
                       vx (:horizontal-speed lander)
                       vy (:vertical-speed lander)
                       rd (* rotation-speed dt)
                       final-speed (fn [a v] (+ (* a dt) v))
                       final-position (fn [p v a]
                                        (+ p
                                           (* v dt)
                                           (/ (* a (* dt dt))
                                              2)))]
                   (update-in s [:lander] merge
                              {:horizontal-speed (final-speed ax vx)
                               :vertical-speed (final-speed ay vy)
                               :x (final-position x vx ax)
                               :y (final-position y vy ay)
                               :rotation (mod (+ (:rotation lander)
                                                 (if (:rotate-cw lander)
                                                   rd
                                                   0)
                                                 (if (:rotate-ccw lander)
                                                   (* rd -1)
                                                   0))
                                              360)})))))

;; UI ;;

(defn pixels
  "Converts meters to pixels."
  [meters]
  (* meters 5))

(defn render-lander [g lander]
  (let [x (pixels (:x lander))
        y (- 0 (pixels (:y lander)))]
    (doto g
      (.rotate
       (Math/toRadians (:rotation lander))
       (+ x 10)
       (+ y 10))
      (.fillRect x y 20 20))))
  
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
    ;(println @state)
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
