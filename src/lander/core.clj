(ns lander.core
  (:gen-class)
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension Polygon Rectangle)
           (java.awt.event KeyAdapter KeyEvent)))

;; This is how often the world is updated and rendered. We'll be aiming for 60
;; frames per second (approx 16ms time-step).
(def time-step (/ 1000 60))

;; Accelerations in m/s^2.
(def gravity-acceleration -1.6)
(def thrust-acceleration 16)

;; Rotation speed defined in degrees per second.
(def rotation-speed 90)

(def state (atom {:lander {:x 30 ; x and y coordinates defined in meters.
                           :y 270
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
  (* meters 1))

(defn translate-y-pixel [y]
  (+ 300
     (* y -1)))

(def level (repeat 400 30))

(defn render-level [g]
  (let [c (count level)
        xs (concat
            (map pixels (range c))
            [(- (pixels c) 1) 0])
        ys (concat (map pixels level) [0 0])
        points (map #(hash-map :x %1 :y %2) xs ys)
        p (Polygon.)]
    (doseq [{:keys [x y]} points]
      (.addPoint p x (translate-y-pixel y)))
    (doto g
      (.fill p))))

(defn render-lander [g lander]
  (let [width (pixels 3)
        height (pixels 4)
        x (pixels (:x lander))
        y (translate-y-pixel (pixels (:y lander)))]
    (doto g
      (.rotate
       (Math/toRadians (:rotation lander))
       (+ x (/ height 2))
       (+ y (/ width 2)))
      (.fillRect x y width height))))
  
(defn render [g]
  (let [state @state]
    (render-level g)
    (render-lander g (:lander state))))

(def panel (doto (proxy [JPanel] []
                   (paintComponent [g]
                     (proxy-super paintComponent g)
                     (render g)))
             (.setPreferredSize (Dimension. 400 300))))

(defn lander-control-listener
  [control key]
  (let [is-key? (fn [event] (= (.getKeyCode event) key))]
    (proxy [KeyAdapter] []
    (keyPressed [event]
      (if (is-key? event)
        (swap! state assoc-in [:lander control] true)))
    (keyReleased [event]
      (if (is-key? event)
        (swap! state assoc-in [:lander control] false))))))

(defn create-gui
  []
  (doto (JFrame. "Lander")
    (.setContentPane panel)
    (.pack)
    (.setVisible true)
    (.addKeyListener (lander-control-listener :thrust KeyEvent/VK_SPACE))
    (.addKeyListener (lander-control-listener :rotate-cw KeyEvent/VK_RIGHT))
    (.addKeyListener (lander-control-listener :rotate-ccw KeyEvent/VK_LEFT))))

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
