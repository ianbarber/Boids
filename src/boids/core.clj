(ns boids.core
	(:use [quil.core])
	(:use [clojure.math.numeric-tower :only abs])
	)
	
;; TODO: test animation	
	
(def wwidth 646)					; Map width
(def wheight 400)					; Map height
(def avoid-dist 30)
(def boid-count 50)
(def boid-diam 5)
(declare boids-list)

(defn create-boid [x y dx dy] {:x x :y y :dx dx :dy dy})

(defn avoid
	"Avoid getting to close to other boids"
	[x y boid-list]
	(reduce 
		(fn [[x y] boid] [(- (:x boid) x) (- (:y boid) y) ])
		[0 0]
		(filter 
			(fn 
				[boid] 
				(< 
				 (+ 
					(abs (- x (:x boid)))
				 	(abs (- y (:y boid))))
					avoid-dist)
				)
				boid-list
			)))

(defn attract 
	"Head towards the average position of the rest of the boids (using all here rather than nearest neighbours)"
	[x y boids]
	(let [boid-count (count boids)
		[sx sy] (reduce (fn [[ix iy] boid] [(+ ix (:x boid)) (+ iy (:y boid))]) [0 0] boids)
		[xav yav] [(/ sx boid-count) (/ sy boid-count)]]
		[(int (/ (- xav x) 10)) (int (/ (- yav y) 10))]
		)
	)
	
(defn align 
	"Head the same direction as most of the other boids"
	[boids]
	(let [boid-count (count boids)
		[sx sy] (reduce (fn [[ix iy] boid] [(+ ix (:dx boid)) (+ iy (:dy boid))]) [0 0] boids)
		[xav yav] [(/ sx boid-count) (/ sy boid-count)]]
			[(int (/ xav 20)) (int (/ yav 20))]
		)
	)  
	
(defn bound 
	"Limit the range"
	[bval bmax]
	(if (< bval 0) 10 (if (> bval bmax) (- bmax 10) bval))
	)

(defn behave
	"The main processing for each boid, apply the three rules and bounds checking"
	[boid boids]
	(let 
		[
			[vx1 vy1] (attract (:x boid) (:y boid) boids)
			[vx2 vy2] (align boids)
			[vx3 vy3] (avoid (:x boid) (:y boid) boids)
			[dx dy] [(/ (+ vx1 vx2 vx3) 3) (/ (+ vy1 vy2 vy3) 3)]
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
  (frame-rate 10)
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
         (fn [] (agent (create-boid (rand-int wwidth) (rand-int wheight) 0 0))))))

(defsketch boidsketch                 
  :title "Boids"  					
  :setup setup                      
  :draw draw                        
  :size [wwidth wheight])        