(ns ray.core
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:require [clojure.math.numeric-tower :as math])
  (:require [mikera.image.core :as image])
  (:require [mikera.image.colours :as colours]))
(set-current-implementation :vectorz)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def pi (* 4 (Math/atan 1)))

;setup output image
(do
  ;;image horizontal and vertical resolution
  (def res-x 500)
  (def res-y 500)
  ;; create a new image
  (def bi (image/new-image res-x res-y))
  ;; gets the pixels of the image, as an int array
  (def pixels (image/get-pixels bi)))

;setup camera
(def camera-location (array [0 1 0]))

(defn camera-ray
  "x and y are coordinates from 0 to 1, starting at the top left of the image"
  [x y]
  (let [camera-direction (array [0 0 1])
        camera-x (array [1 0 0])
        camera-y (array [0 1 0])]
    (+ (* 2 (- x 0.5) camera-x) (* -2 (- y 0.5) camera-y) camera-direction)))


;checkerboard texture: returns 1 or 0 for x,y coordinates on checker-spacing size grid
(defn checker-texture [x y checker-spacing]
  (let [xs (math/round (/ (mod x checker-spacing) checker-spacing))
        ys (math/round (/ (mod y checker-spacing) checker-spacing))]
    (mod (+ xs ys) 2)))

(defn ray-plane [P0 P1]
  ;Ray-(ground)Plane Intersection:
  ;where the ray is defined as P0+tP1 with parameter t
  ;and the plane is the x-z (ground) plane (y=0)
  ;P0 and P1 are matrix.core vectorz arrays (vectors)
  (if-not (zero? (mget P1 1))
    (let [t (- (/ (mget P0 1) (mget P1 1)))]
      (if (pos? t)
        (+ P0 (* P1 t))))))


(do
  (def r-rand (rand))
  (def g-rand (rand))
  (def b-rand (rand))
  )

(dotimes [i (* res-x res-y)]
  (let [x (mod i res-x)
        y (quot i res-x)
        plane-intersection (ray-plane camera-location (camera-ray (/ x res-x) (/ y res-y)))]
    (if plane-intersection
      (let [u-tex (mget plane-intersection 0)
            v-tex (mget plane-intersection 2)
            pixel-intensity (int (checker-texture u-tex v-tex 1))]
        (aset ^ints pixels i (colours/rgb (* r-rand pixel-intensity) (* g-rand pixel-intensity) (* b-rand pixel-intensity)))))))


(comment (dotimes [i (* res-x res-y)]
           (aset pixels i (colours/rgb (* r-rand) (* g-rand) (* b-rand)))))


;; update the image with the newly changed pixel values
(image/set-pixels bi pixels)

;; view our new work of art
;; the zoom function will automatically interpolate the pixel values
(image/show bi :zoom (/ 500 res-x) :title "Isn't it beautiful?")



(comment
  (def A (identity-matrix res-x))
  ;(pm A)
  ;; fill some random pixels with colours
  (dotimes [i (* res-x res-y)] (aset pixels i (colours/rgb (nth (eseq A) i) (nth (eseq A) i) (nth (eseq A) i))))
  (pm pixels)


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

  )
