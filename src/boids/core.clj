(ns boids.core
	(:use [quil.core]))
	
(def wwidth 646)					; Map width
(def wheight 400)					; Map height
(def avoid-dist 20)					; Stay this far apart
(def col-dist 35)					; Stay this far away from the columns
(def col-size 30) 					; Column size
(def infl-dist 100)					; Boids look at other boids in this distance
(def boid-count 50)					; How many boids to have
(def boid-diam 5)					; Size of a boid
(def max-speed 6)					; How fast a boid can go 

(def column-list
	(take 5
		(repeatedly 
			(fn [] {:x (rand-int wwidth) :y (rand-int wheight)})
		)))

(defn create-boid 
	"Return a new boid"
	[x y dx dy colour] 
	{:x x :y y :dx dx :dy dy :colour colour})

(defn close-boids
	"Get a list of boids which are close"
	[x y boids dist]
	(filter 
		(fn [boid] 
		    (< (+ (Math/abs (int(- x (:x boid)))) (Math/abs (int (- y (:y boid))))) dist) )
		boids
	))
	
(def boids-list
	(take boid-count
		(repeatedly
			(fn [] (agent (create-boid 
				(rand-int wwidth) 
				(rand-int wheight) 
				(- (rand-int 10) 5) 
				(- (rand-int 10) 5) 
			[(rand-int 255) (rand-int 255) (rand-int 255)]
		))))))

(defn avoid
	"Avoid getting to close to other boids"
	[x y boid-list column-list]
	(let [
	    [x1 y1] (reduce 
			(fn [[sx sy] boid] [(+ sx (- x (:x boid))) (+ sy (- y (:y boid)))])
			[0 0]
			(close-boids x y boid-list avoid-dist)
		)
		[x2 y2] (reduce 
			(fn [[sx sy] column] [(+ sx (- x (:x column))) (+ sy (- y (:y column)))])
			[0 0]
			(close-boids x y column-list col-dist)
		)]
		[(+ (/ x1 4) x2) (+ (/ y1 4) y2)]
	))

(defn attract 
	"Head towards the average position of the rest of the boids (using all here rather than nearest neighbours)"
	[x y boids influence]
	(let [
	    cboids (close-boids x y boids influence)
	    boid-count (if (= (count cboids) 0) 1 (count cboids))
		[sx sy] (reduce (fn [[ix iy] boid] [(+ ix (:x boid)) (+ iy (:y boid))]) [0 0] cboids)
		[xav yav] [(/ sx boid-count) (/ sy boid-count)]]
		[(int (/ (- xav x) 30)) (int (/ (- yav y) 30))]
		))
	
(defn align 
	"Head the same direction as most of the other boids"
	[x y boids influence]
	(let [
	    cboids (close-boids x y boids influence)
	    boid-count (+ (count cboids) 1) ;; allow for 0
		[sx sy] (reduce (fn [[ix iy] boid] [(+ ix (:dx boid)) (+ iy (:dy boid))]) [0 0] cboids)
		[xav yav] [(/ sx boid-count) (/ sy boid-count)]]
			[(int (/ xav 2)) (int (/ yav 2))]
		))  

(defn calc-speed 
	[[x y]]
	(Math/sqrt (+ (* x x) (* y y))))	

(defn bound 
	"Limit the range"
	[bval dval bmax]
	(if (< bval 0) (/ max-speed 2) (if (> bval bmax) (- 0 (/ max-speed 2)) dval)))
	
(defn bound-pos
	"Bound the velocity based on the position"
	[boid [dx dy]]
	[(bound (+ (:x boid) dx) dx wwidth) (bound (+ (:y boid) dy) dy wheight)])

(defn bound-speed
	"Limit the speed of a boid"
	[[x y]]
	(let [bspeed (calc-speed [x y])]
		(if (> bspeed max-speed) 
			[(* max-speed (/ x bspeed)) (* max-speed (/ y bspeed))]
			[x y]
			)))

(defn behave
	"The main processing for each boid, apply the three rules and bounds checking"
	[boid boids influence]
	(let 
		[
			[vx1 vy1] (attract (:x boid) (:y boid) boids influence)
			[vx2 vy2] (align (:x boid) (:y boid) boids  influence)
			[vx3 vy3] (avoid (:x boid) (:y boid) boids column-list)
			[vx4 vy4] [(/ (- (/ wwidth 2) (:x boid)) 300) (/ (- (/ wheight 2) (:y boid)) 300)] ;; tend to center
			[dx dy] (bound-pos boid (bound-speed [(+ (:dx boid) (/ (+ vx1 vx2 vx3 vx4) 4)) (+ (:dy boid) (/ (+ vy1 vy2 vy3 vy4) 4))]))
			[newx newy] [(+ (:x boid) dx) (+ (:y boid) dy)]
		]
		(create-boid newx newy dx dy (:colour boid))	
	))
	
(defn draw-boid
	"Draw a boid using quil"
	[boid]
		(let [[r g b] (:colour boid)]
			(stroke (color r g b))
			(fill (color r g b))) ;; fill
			(ellipse (:x boid) (:y boid) boid-diam boid-diam)
	)

(defn setup []
	(smooth)                          ;;Turn on anti-aliasing
	(frame-rate 24)
	(background 255))

(defn draw []
	;; Tell the boid agents to update
	(doseq [agent-boid boids-list]
		(send-off agent-boid behave (remove (partial = @agent-boid) (map deref boids-list)) infl-dist))

	;; Blank the background
	(stroke-weight 0)
	(fill 255) 
	(rect 0 0 wwidth wheight)
	
	(doseq [col column-list] (do 
		(stroke-weight 3)
		(stroke 230)
		(fill 200)
		(ellipse (:x col) (:y col) col-size col-size)))
	
	;; Draw the boids
	(doseq [boid (map deref boids-list)] (draw-boid boid)))

(defn -main []
    (defsketch boidsketch                 
      :title "Boids"  					
      :setup setup                      
      :draw draw                        
      :size [wwidth wheight]))