(ns boids.core
	(:use [quil.core])
	)
	
;; TODO: test animation	
	
(def wwidth 646)					; Map width
(def wheight 400)					; Map height
(def avoid-dist 20)
(def infl-dist 200)
(def boid-count 50)
(def boid-diam 5)
(def speed 5)
(declare boids-list)

(defn create-boid [x y dx dy] {:x x :y y :dx dx :dy dy})

(defn close-boids
    "Get a list of boids which are close"
    [x y boids-list dist]
    (filter 
		(fn [boid] 
		    (< (+ (Math/abs (int(- x (:x boid)))) (Math/abs (int (- y (:y boid))))) dist) )
		boids-list
	))

(defn avoid
	"Avoid getting to close to other boids"
	[x y boid-list]
	(let [[x y](reduce 
		(fn [[sx sy] boid] [(+ sx (- x (:x boid))) (+ sy (- y (:y boid)))])
		[0 0]
		(close-boids x y boid-list avoid-dist)
		)]
		[(/ x speed) (/ y speed)]
	))

(defn attract 
	"Head towards the average position of the rest of the boids (using all here rather than nearest neighbours)"
	[x y boids]
	(let [
	    cboids (close-boids x y boids infl-dist)
	    boid-count (+ (count cboids) 1) ;; allow for 0
		[sx sy] (reduce (fn [[ix iy] boid] [(+ ix (:x boid)) (+ iy (:y boid))]) [0 0] cboids)
		[xav yav] [(/ sx boid-count) (/ sy boid-count)]]
		[(int (/ (- xav x) 50)) (int (/ (- yav y) 50))]
		)
	)
	
(defn align 
	"Head the same direction as most of the other boids"
	[x y boids]
	(let [
	    cboids (close-boids x y boids infl-dist)
	    boid-count (+ (count cboids) 1) ;; allow for 0
		[sx sy] (reduce (fn [[ix iy] boid] [(+ ix (:dx boid)) (+ iy (:dy boid))]) [0 0] cboids)
		[xav yav] [(/ sx boid-count) (/ sy boid-count)]]
			[(int (/ xav speed)) (int (/ yav speed))]
		)
	)  
	
(defn bound 
	"Limit the range"
	[bval bmax]
	(if (< bval 0) 10 (if (> bval bmax) (- bmax 10) bval))
	)
	
;; this is broken!
(defn bound-speed
    "Limit the speed of a boid"
    [[x y]]
    (let [bspeed (- 20 (+ (Math/abs (int x)) (Math/abs (int y))))]
        (if (< bspeed 0) 
            [(- x bspeed) (- y bspeed)]
            [x y]
            )
    ))

(defn behave
	"The main processing for each boid, apply the three rules and bounds checking"
	[boid boids]
	(let 
		[
			[vx1 vy1] (attract (:x boid) (:y boid) boids)
			[vx2 vy2] (align (:x boid) (:y boid) boids)
			[vx3 vy3] (avoid (:x boid) (:y boid) boids)
			;; this is also a bit broken!
			[dx dy] (bound-speed [(+ (:dx boid) (/ (+ vx1 vx2 vx3) 3)) (+ (:dy boid) (/ (+ vy1 vy2 vy3) 3))])
			[newx newy] [(+ (:x boid) dx) (+ (:y boid) dy)]
		]
		(create-boid (bound newx wwidth) (bound newy wheight) dx dy)	
	))
	
(defn draw-boid
	"Draw a boid using quil"
	[boid]
		(stroke 50) ;; dark border
		(fill 100) ;; lighter but still dark fill
		(ellipse (:x boid) (:y boid) boid-diam boid-diam)
	)

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 25)
  (background 255))

(defn draw []
	;; Tell the boid agents to update
	(doseq [agent-boid boids-list]
	      (send-off agent-boid behave (remove (partial = @agent-boid) (map deref boids-list))))

	;; Blank the background
	(stroke-weight 0)
	(fill 255) 
	(rect 0 0 wwidth wheight)
	
	;; Draw the boids
	(doseq [boid (map deref boids-list)] (draw-boid boid)))

(def boids-list
  (take boid-count
        (repeatedly
         (fn [] (agent (create-boid (rand-int wwidth) (rand-int wheight) (- (rand-int 10) 5) (- (rand-int 10) 5)))))))

(defn -main []
    (defsketch boidsketch                 
      :title "Boids"  					
      :setup setup                      
      :draw draw                        
      :size [wwidth wheight]))