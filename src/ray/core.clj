(ns ray.core
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:require [clojure.math.numeric-tower :as math])
  (:require [mikera.image.core :as image])
  (:require [mikera.image.colours :as colours])
  (:require [ray.colours :as c])
  (:require [mikera.vectorz.core :as v])
  (:import [java.awt.image BufferedImage])
  (:import [mikera.image])
  (:import [mikera.vectorz Vector3 Vector4 AVector Vectorz])
  )
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
    (mod (+ xs ys) 2.0)))

(defn ray-plane [P0 P1]
  ;Ray-(ground)Plane Intersection:
  ;where the ray is defined as P0+tP1 with parameter t
  ;and the plane is the x-z (ground) plane (y=0)
  ;P0 and P1 are matrix.core vectorz arrays (vectors)
  (if-not (zero? (mget P1 1))
    (let [t (- (/ (mget P0 1) (mget P1 1)))]
      (if (pos? t)
        (+ P0 (* P1 t))))))


(defn new-image
  "Creates a new blank image"
  ([w h]
   (mikera.gui.ImageUtils/newImage (int w) (int h))))

(defn render []
  (let [width (int res-x)
        height (int res-y)
        colour-result (v/vec4 [0 0 0 1])
        r-rand (rand)
        g-rand (rand)
        b-rand (rand)
        ^BufferedImage im (new-image width height)]
    (dotimes [ix width]
      (dotimes [iy height]
        (let [plane-intersection (ray-plane camera-location (camera-ray (/ ix res-x) (/ iy res-y)))
              ]
          (if plane-intersection
            (let [u-tex (mget plane-intersection 0)
                  v-tex (mget plane-intersection 2)
                  temp (v/vec3)
                  pixel-intensity (checker-texture u-tex v-tex 1)
                  ]
              (.setValues colour-result ^double pixel-intensity ^double pixel-intensity ^double pixel-intensity 1.0)
              (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))))
    im))

(time (image/show (render) :zoom (/ 500 res-x) :title "Isn't it beautiful?"))

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
