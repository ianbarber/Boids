(ns boids.test.core
  (:use [boids.core] :reload)
  (:use [clojure.test]))

(deftest test-create-boid
  (is (= 10 (:x (create-boid 10 0 0 0 []))) "Create boid does not include x val"))

(deftest test-close-boids
    (is (= 1 (count (close-boids 60 60 #{(create-boid 80 30 10 10 [])} 100 ))) "Close boids does not include expected boids"))

(deftest test-avoid
    (let [[x y]  (avoid 60 55 #{(create-boid 65 60 10 10 [])} [])]
        (is (< x 0) "X val of avoid should be negative for avoidance")
        (is (< y 0) "Y val of avoid should be negative for avoidance")
    )
    (let [[x y]  (avoid 60 55 #{(create-boid 55 50 10 10 [])} [])]
        (is (> x 0) "X val of avoid should be positive for avoidance")
        (is (> y 0) "Y val of avoid should be positive for avoidance")
    )
    (let [[x y]  (avoid 60 55 #{(create-boid 65 50 10 10 [])} [])]
        (is (< x 0) "X val of avoid should be negative for avoidance")
        (is (> y 0) "Y val of avoid should be positive for avoidance")
    ))
    
(deftest test-attract
    (let [[x y]  (attract 60 60 #{(create-boid 90 30 10 10 [])} 200)]
        (is (> x 0) "Boids should be positive X if the center of mass is > x")
        (is (< y 0) "Boids should be negative Y if the center of ass is < y")
    ))
    
(deftest test-align
    (let [[x y]  (align 60 60 #{(create-boid 80 30 10 -10 [])} 100)]
        (is (> x 0) "Boids should be positive X")
        (is (< y 0) "Boids should be negative Y")
    ))    

(deftest test-bound
    (is (< 0 (bound -20 10 100)) "Bound should clip to +")
    (is (> 0 (bound 110 10 100)) "Bound should clip to -")
    )

(deftest test-behave
    (let [boid  (behave (create-boid 50 50 0 0 []) #{(create-boid 100 100 10 10 [])} 200)]
        (is (> (:x boid) 49) "Boids should be positive X")
        (is (> (:y boid) 49) "Boids should be positive Y")
        (is (> (:dx boid) 0) "Boids should be positive velocity X")
        (is (> (:dy boid) 0) "Boids should be positive velocity Y")
    )
    (let [boid  (behave (create-boid 50 50 0 0 []) #{(create-boid 0 0 -10 -10 [])} 200)]
        (is (< (:x boid) 50) "Boids should be negative X")
        (is (< (:y boid) 50) "Boids should be negative Y")
        (is (< (:dx boid) 0) "Boids should be negative velocity X")
        (is (< (:dy boid) 0) "Boids should be negative velocity Y")
    ))