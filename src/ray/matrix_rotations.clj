(ns ray.matrix-rotations
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:require [clojure.math.numeric-tower :as math]))






;dot product benchmark
(comment
  (defn dot-product [x y]
    (reduce + (map * x y)))

  (def v [1 2 3])
  (def v8 (array [1 2 3]))

  (time (dotimes [_ 1e5] (dot-product v v)))
  (time (dotimes [_ 1e5] (dot v v)))

  (time (dotimes [_ 1e5] (dot-product v8 v8)))
  (time (dotimes [_ 1e5] (dot v8 v8))))

;2D rotation matrix test
(comment
  (def A (matrix [[1 2] [3 4]]))
  (defn rotation-matrix-2D [theta] (matrix [[(Math/cos theta) (- (Math/sin theta))] [(Math/sin theta) (Math/cos theta)]]))
  (mmul (rotation-matrix-2D (* pi 0.5)) (array [1 0]))
  (mmul (rotation-matrix-2D (* pi 0.5)) A))

;3d rotation matrices
(defn rotate-x [a] (matrix [[1 0 0]
                            [0 (Math/cos a) (Math/sin a)]
                            [0 (- (Math/sin a)) (Math/cos a)]]))
(defn rotate-y [b] (matrix [[(Math/cos b) 0 (- (Math/sin b))]
                            [0 1 0]
                            [(Math/sin b) 0 (Math/cos b)]]))
(defn rotate-z [c] (matrix [[(Math/cos c) (Math/sin c) 0]
                            [(- (Math/sin c)) (Math/cos c) 0]
                            [0 0 1]]))


