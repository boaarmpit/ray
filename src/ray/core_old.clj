(ns ray.core-old
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:require [clojure.math.numeric-tower :as math])
  (:require [mikera.image.core :as image])
  ;(:require [mikera.image.colours :as colours])


  ;(:require [mikera.vectorz.core :as v])
  ;(:require [mikera.vectorz.matrix :as m])

  (:require [ray.colours :as c])
  (:require [mikera.vectorz.core :as v])
  (:import [java.awt.image BufferedImage])
  ;(:import [mikera.image])
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
  (def res-y 500))

;setup camera
(def camera-location (array [0 1 0]))

(defn camera-ray
  "x and y are coordinates from 0 to 1, starting at the top left of the image"
  [x y]
  (let [camera-direction (array [0 0 1])
        camera-x (array [1 0 0])
        camera-y (array [0 1 0])]
    (+ (* 2 (- x 0.5) camera-x) (* -2 (- y 0.5) camera-y) camera-direction))
  )


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


(defn ray-sphere [P0 P1 C0]

  )



(defn new-image
  "Creates a new blank image"
  ([w h]
    (mikera.gui.ImageUtils/newImage (int w) (int h))))

(defn render []
  (let [colour-result (v/vec4 [0 0 0 1])
        ^Vector3 camera-direction (v/vec3 [0 0 1])
        ^Vector3 camera-x (v/vec3 [1 0 0])
        ^Vector3 camera-y (v/vec3 [0 1 0])

        ;MUTABLES
        ^Vector3 dir (v/vec3 [0 0 0])
        ^BufferedImage im (new-image res-x res-y)]

    (dotimes [ix res-x]
      (dotimes [iy res-y]
        (.set dir camera-direction)
        (v/add-multiple! dir camera-x (* 2 (- (/ (double ix) res-x) 0.5)))
        (v/add-multiple! dir camera-y (* -2 (- (/ (double iy) res-y) 0.5)))
        (v/normalise! dir)
        (let [plane-intersection (ray-plane camera-location dir)]
          (if plane-intersection
            (let [u-tex (mget plane-intersection 0)
                  v-tex (mget plane-intersection 2)
                  pixel-intensity (checker-texture u-tex v-tex 1)
                  ]
              (.setValues colour-result ^double pixel-intensity ^double pixel-intensity ^double pixel-intensity 1.0
                          )
              (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))))
    im))

(time (image/show (render) :zoom (/ 500 res-x) :title "Isn't it beautiful?"))
(time (render))


(defn render-old []
  (let [colour-result (v/vec4 [0 0 0 1])
        r-rand (rand)
        g-rand (rand)
        b-rand (rand)
        camera-direction (array [0 0 1])
        camera-x (array [1 0 0])
        camera-y (array [0 1 0])

        ;MUTABLES
        ^Vector3 dir (v/vec3 [0 0 0])
        ^BufferedImage im (new-image res-x res-y)]

    (dotimes [ix res-x]
      (dotimes [iy res-y]
        (.set dir camera-direction)
        (v/add-multiple! dir camera-x (* 2 (- (/ ix res-x) 0.5)))
        (v/add-multiple! dir camera-y (* -2 (- (/ iy res-y) 0.5)))
        (v/normalise! dir)
        (let [plane-intersection (ray-plane camera-location dir)]
          (if plane-intersection
            (let [u-tex (mget plane-intersection 0)
                  v-tex (mget plane-intersection 2)
                  pixel-intensity (checker-texture u-tex v-tex 1)]
              (.setValues colour-result ^double (* r-rand pixel-intensity) ^double (* g-rand pixel-intensity) ^double (* b-rand pixel-intensity) 1.0)
              (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))))
    im))