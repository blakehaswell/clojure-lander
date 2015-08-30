(ns lander.core
  (:gen-class)
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension Polygon Rectangle)
           (java.awt.event KeyAdapter KeyEvent)))

;; This is how often the world is updated and rendered. We'll be aiming for 60
;; frames per second (approx 16ms time-step).
(def time-step (/ 1000 60))

(def world-width 256)
(def world-height 192)

(defn new-level
  [state]
  (-> state
      (assoc-in [:game-state] :playing)
      (assoc-in [:level] (generate-level))
      (assoc-in [:lander] (merge (:lander state) {:x 30
                                                  :y 162
                                                  :rotation 0
                                                  :vertical-speed 0
                                                  :horizontal-speed 0}))))

;; TODO: Other stuff like resetting scores and timers and so on.
(defn new-game
  [state]
  (-> state
      new-level
      (update-in [:lander] #(merge % {:fuel 100}))))

(defn init-game
  []
  (-> {}
      (new-game)
      (assoc-in [:game-state] :init)))

(defmulti update-game-state :game-state)

(defmethod update-game-state :playing
  [{{lx :x ly :y vx :horizontal-speed vy :vertical-speed} :lander
    level :level
    :as state}]
  (let [_lx (if (integer? lx) lx (Math/round lx))
        gy (->> level
                (filter (comp (partial = _lx) :x))
                first
                :y)]
    (println ly gy vx vy)
    (if (<= ly gy)
      (update-in state [:game-state] (fn [_] (if (and (<= vx 10)
                                                      (<= vy 10))
                                               :landed
                                               :crashed)))
      
      state)))

(defmethod update-game-state :default [state] state)

(defmulti start-game :game-state)

(defmethod start-game :landed
  [state]
  (new-level state))

(defmethod start-game :crashed
  [state]
  (new-game state))

(defmethod start-game :init
  [state]
  (assoc-in state [:game-state] :playing))

(defmethod start-game :default [state] state)

;; Accelerations in m/s^2.
(def gravity-acceleration -1.6)
(def thrust-acceleration 16)

;; Rotation speed defined in degrees per second.
(def rotation-speed 90)

(defn update-lander
  [lander]
  (let [x (:x lander)
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
    (merge lander
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
                           360)})))

(defmulti update-physics :game-state)

(defmethod update-physics :playing
  [state]
  (update-in state [:lander] update-lander))

(defmethod update-physics :default [state] state)

;; State ;;

(def state (atom (init-game)))

(defn update-state []
  (swap! state (fn [s] (-> s
                           update-physics
                           update-game-state))))

(defn advance-game-state []
  (println "Advancing game state")
  (let [game-state (:game-state @state)]
    (if (not (= :playing game-state))
      (swap! state start-game))))

;; UI ;;

(def screen-width 512)
(def screen-height 384)

(defn to-pixels
  "Converts meters to pixels."
  [meters]
  (* meters (/ screen-width world-width)))

(defn to-screen-coords
  "Converts co-ordinates defined in meters into on-screen co-ordinates."
  [{:keys [x y]}]
  {:x (to-pixels x)
   :y (+ screen-height
         (* (to-pixels y) -1))})

(defn average
  [& ns]
  (/ (reduce + ns)
     (count ns)))

(defn split-line
  [d {:keys [x1 y1 x2 y2]}]
  (let [xm (average x1 x2)
        ;; r is a random number between -1 and 1.
        r (- (rand 2)  1)
        ym (+ (average y1 y2)
              (* r d))]
    [{:x1 x1 :y1 y1 :x2 xm :y2 ym}
     {:x1 xm :y1 ym :x2 x2 :y2 y2}]))

(defn generate-level
  []
  ;; r is somewhere between 30 and 150.
  (let [r (+ (* (rand) 120) 30)]
    (print r)
    (loop [lines [{:x1 0
                   :y1 r
                   :x2 256
                   :y2 (- 150 r)}]
           d 125
           ;; Looping 8 times gives us 256 lines.
           count 8]
      (if (= 0 count)
        (conj (map (fn [line]
                     {:x (:x2 line)
                      :y (:y2 line)})
                   lines)
              (let [line (first lines)]
                {:x (:x1 line)
                 :y (:y1 line)}))
        (recur (mapcat (partial split-line d) lines)
               (* d 0.4)
               (- count 1))))))

(defn render-level [g level]
  (let [p (Polygon.)]
    (doseq [point level]
      (let [{:keys [x y]} (to-screen-coords point)]
        (.addPoint p x y)))
    (doto p
      (.addPoint screen-width screen-height)
      (.addPoint 0 screen-height))
    (.fill g p)))

;; TODO: Render lander with origin in bottom center, not top left.
(defn render-lander [g lander]
  (let [width (to-pixels 3)
        height (to-pixels 4)
        {:keys [x y]} (to-screen-coords lander)]
    (doto g
      (.rotate
       (Math/toRadians (:rotation lander))
       (+ x (/ height 2))
       (+ y (/ width 2)))
      (.fillRect x y width height))))
  
(defn render [g]
  (let [state @state]
    (render-level g (:level state))
    (render-lander g (:lander state))))

(def panel (doto (proxy [JPanel] []
                   (paintComponent [g]
                     (proxy-super paintComponent g)
                     (render g)))
             (.setPreferredSize (Dimension. screen-width screen-height))))

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
    (.addKeyListener (proxy [KeyAdapter] []
                       (keyPressed [event]
                         (if (= (.getKeyCode event) KeyEvent/VK_SPACE)
                           (advance-game-state)))))
    (.addKeyListener (lander-control-listener :thrust KeyEvent/VK_SPACE))
    (.addKeyListener (lander-control-listener :rotate-cw KeyEvent/VK_RIGHT))
    (.addKeyListener (lander-control-listener :rotate-ccw KeyEvent/VK_LEFT))))

(defn start-loop []
  (let [start-time (System/currentTimeMillis)]
    ;(println (:game-state @state))
    (update-state)
    (.repaint panel)
    (Thread/sleep (max 0
                       (- (+ time-step start-time)
                          (System/currentTimeMillis))))
    (recur)))


(defn -main
  [& args]
  (create-gui)
  (start-loop))
