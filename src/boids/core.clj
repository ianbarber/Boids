(ns boids.core
	(:use [quil.core])
	)
	
(def wwidth 646)					; Map width
(def wheight 400)					; Map height
(def avoid-dist 30)                 ; Stay this far apart
(def infl-dist 100)                 ; Boids look at other boids in this distance
(def boid-count 50)                 ; How many boids to have
(def boid-diam 5)                   ; Size of a boid
(def max-speed 6)                  ; How fast a boid can go 
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
		[(/ x 10) (/ y 10)]
	))

(defn attract 
	"Head towards the average position of the rest of the boids (using all here rather than nearest neighbours)"
	[x y boids]
	(let [
	    cboids (close-boids x y boids infl-dist)
	    boid-count (+ (count cboids) 1) ;; allow for 0
		[sx sy] (reduce (fn [[ix iy] boid] [(+ ix (:x boid)) (+ iy (:y boid))]) [0 0] cboids)
		[xav yav] [(/ sx boid-count) (/ sy boid-count)]]
		[(int (/ (- xav x) 40)) (int (/ (- yav y) 40))]
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
			[(int (/ xav 3)) (int (/ yav 3))]
		)
	)  
	
(defn bound 
	"Limit the range"
	[bval dval bmax]
	(if (< bval 0) (/ max-speed 2) (if (> bval bmax) (- 0 (/ 2 max-speed)) dval))
	)
	
(defn calc-speed 
    [[x y]]
    (Math/sqrt (+ (* x x) (* y y)))
    )	
	
(defn bound-pos
    "Bound the velocity based on the position"
    [boid [dx dy]]
    (let 
        [   newx (+ (:x boid) dx) 
            newy (+ (:y boid) dy)]
        [(bound newx dx wwidth) (bound newy dy wheight)]
    )    
)

(defn bound-speed
    "Limit the speed of a boid"
    [[x y]]
    (let [bspeed (calc-speed [x y])]
        (if (> bspeed max-speed) 
            [(* max-speed (/ x bspeed)) (* max-speed (/ y bspeed))]
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
			[dx dy] (bound-pos boid (bound-speed [(+ (:dx boid) (/ (+ vx1 vx2 vx3) 3)) (+ (:dy boid) (/ (+ vy1 vy2 vy3) 3))]))
			[newx newy] [(+ (:x boid) dx) (+ (:y boid) dy)]
		]
		(create-boid newx newy dx dy)	
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
  (frame-rate 24)
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