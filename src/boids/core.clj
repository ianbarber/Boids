(ns boids.core
	(:use [quil.core]))
	
(def mousepos (atom [0 0]))			; Hold the mouse coordinates
(def wwidth 646)					; Map width
(def wheight 400)					; Map height
(def avoid-dist 20)					; Stay this far apart
(def col-dist 45)					; Stay this far away from the columns
(def col-size 30) 					; Column size
(def infl-dist 100)					; Boids look at other boids in this distance
(def boid-count 50)					; How many boids to have
(def boid-diam 5)					; Size of a boid
(def max-speed 10)					; How fast a boid can go 

(def column-list
	(take 3
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
	(atom (take boid-count
		(repeatedly
			(fn [] (create-boid 
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
		[(+ (/ x1 2) x2) (+ (/ y1 2) y2)]
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
			[vx1 vy1] (bound-speed (attract (:x boid) (:y boid) boids influence))
			;[vx1 vy1] [0 0]
			[vx2 vy2] (bound-speed (align (:x boid) (:y boid) boids  influence))
			;[vx2 vy2] [0 0]
			[vx3 vy3] (avoid (:x boid) (:y boid) boids column-list)
			;;[vx3 vy3] [0 0]
			[mx my] @mousepos
			[vx4 vy4] (if (> mx 0) (bound-speed [(/ (- mx (:x boid)) 40) (/ (- my (:y boid)) 40)]) [0 0])
			[dx1 dy1] [(/ (+ vx1 vx2 vx3 vx4) 4) (/ (+ vy1 vy2 vy3 vy4) 4)]
			[dx dy] (bound-pos boid (bound-speed [(+ (:dx boid) dx1) (+ (:dy boid) dy1)]))
			;;[dx dy] (bound-pos boid [(+ (:dx boid) dx1) (+ (:dy boid) dy1)])
			
			[newx newy] [(int (+ (:x boid) dx)) (int (+ (:y boid) dy))]
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
	;; Update the boids position
	(reset! boids-list (doall (map 
		#(behave % (remove (partial = %) @boids-list) infl-dist) @boids-list)))

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
	(doseq [boid @boids-list] (draw-boid boid)))

(defn mouse-moved [] 
	(let [x (mouse-x)
		 y (mouse-y)]
		(reset! mousepos [x y])
		)
	)

(defn -main []
    (defsketch boidsketch                 
      :title "Boids"  					
      :setup setup                      
      :draw draw  
      :size [wwidth wheight]
	  :mouse-moved mouse-moved))