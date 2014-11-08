(ns ray.core
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:require [clojure.math.numeric-tower :as math])
  (:require [mikera.image.core :as image])
  (:require [ray.colours :as c])
  (:require [mikera.vectorz.core :as v])
  (:import [java.awt.image BufferedImage])
  (:import [mikera.vectorz Vector3 Vector4 AVector Vectorz]))
(set-current-implementation :vectorz)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def pi (* 4 (Math/atan 1)))

;setup output image
(do
  ;;image horizontal and vertical resolution
  (def res-x 500)
  (def res-y 500))


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
    (* 0.7 (mod (+ xs ys) 2.0))))

(defn ray-plane
  "Ray-(ground)Plane Intersection:
  where the ray is defined as P0+tP1 with parameter t
  and the plane is the x-z (ground) plane (y=0)
  P0 and P1 are mikera.vectorz Vector3s (vectors)"
  [P0 P1]
  (if-not (zero? (mget P1 1))
    (let [t (- (/ (mget P0 1) (mget P1 1)))]
      (if (pos? t)
        (+ P0 (* P1 t))))))


(defn ray-sphere
  "Ray-Sphere Intersection:
   The ray is defined as P0+tP1 with parameter t
   The sphere is centered at C with radius r"
  [P0 P1 C r]
  (v/normalise! P1)
  (let [a (v/dot P1 P1)
        b (* 2 (v/dot P1 (- P0 C)))
        c (- (v/dot (- P0 C) (- P0 C)) (* r r))
        det (- (* b b) (* 4 a c))
        hit (pos? det)
        [intersection normal] (if hit (let [t (/ (- 0 b (math/sqrt (- (* b b) (* 4 a c)))) (* 2 a))
                                            intersection (+ P0 (* t P1))
                                            normal (v/normalise (- intersection C))]
                                        [intersection normal]))]
    {:sphere-intersection intersection :normal normal}))


(defn new-image
  "Creates a new blank image"
  ([w h]
   (mikera.gui.ImageUtils/newImage (int w) (int h))))

(defn render-sphere []
  (let [colour-result (v/vec4 [0 0 0 1])
        ^Vector3 camera-location (v/vec3 [0 1 0])
        ^Vector3 camera-direction (v/vec3 [0 0 1])
        ^Vector3 camera-x (v/vec3 [1 0 0])
        ^Vector3 camera-y (v/vec3 [0 1 0])
        ^Vector3 sphere-origin (v/vec3 [0 0.5 3])
        sphere-radius 1.5

        ;MUTABLES
        ^Vector3 dir (v/vec3 [0 0 0])
        ^BufferedImage im (new-image res-x res-y)]

    (dotimes [ix res-x]
      (dotimes [iy res-y]
        (.set dir camera-direction)
        (v/add-multiple! dir camera-x (* 2 (- (/ (double ix) res-x) 0.5)))
        (v/add-multiple! dir camera-y (* -2 (- (/ (double iy) res-y) 0.5)))
        (v/normalise! dir)
        (let [{:keys [sphere-intersection normal]} (ray-sphere camera-location dir sphere-origin sphere-radius)]
          (if sphere-intersection
            (let [pixel-intensity (+ (/ (math/expt (- (v/mget normal 2)) 30) 1.5) (/ (- (v/mget normal 2)) 3))]
              (.setValues colour-result pixel-intensity pixel-intensity pixel-intensity 1.0)
              (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))))
    im))



(defn render-reflective-sphere []
  (let [colour-result (v/vec4 [0 0 0 1])
        ^Vector3 camera-location (v/vec3 [0 0.2 0])
        ^Vector3 camera-direction (v/vec3 [1 0 1])
        ^Vector3 camera-x (v/vec3 [0.5 0 -0.5])
        ^Vector3 camera-y (v/vec3 [0 0.707 0])
        ^Vector3 sphere-origin (v/vec3 [3 1 3])
        ^Vector3 light-direction (v/normalise (v/vec3 [-1 1 -2]))
        sphere-radius 1

        ;MUTABLES
        ^Vector3 dir (v/vec3 [0 0 0])
        ^BufferedImage im (new-image res-x res-y)]

    (dotimes [ix res-x]
      (dotimes [iy res-y]
        (.set dir camera-direction)
        (v/add-multiple! dir camera-x (* 2 (- (/ (+ (* 1 (rand)) (double ix)) res-x) 0.5)))
        (v/add-multiple! dir camera-y (* -2 (- (/ (+ (* 1 (rand)) (double iy)) res-y) 0.5)))
        (v/normalise! dir)
        (let [{:keys [sphere-intersection normal]} (ray-sphere camera-location dir sphere-origin sphere-radius)
              plane-intersection (ray-plane camera-location dir)]
          (if sphere-intersection
            (let [reflected-direction (- dir (* 2 (dot dir normal) normal))
                  plane-intersection2 (ray-plane sphere-intersection reflected-direction)]
              (let [pixel-intensity (if plane-intersection2
                                      (let [u-tex (mget plane-intersection2 0)
                                            v-tex (mget plane-intersection2 2)
                                            pixel-intensity (+ (* 0.5 (checker-texture u-tex v-tex 1))
                                                               (+ (* 0.5 (math/expt (Math/sin (* pi 0.5 (dot normal (v/vec3 light-direction)))) 50))
                                                                  (* 0.3 (math/expt (Math/sin (* pi 0.5 (dot normal (v/vec3 light-direction)))) 5))))]
                                        pixel-intensity)
                                      (+ (* 0.5 (math/expt (Math/sin (* pi 0.5 (dot normal (v/vec3 light-direction)))) 50))
                                         (* 0.3 (math/expt (Math/sin (* pi 0.5 (dot normal (v/vec3 light-direction)))) 5))))]
                (.setValues colour-result ^double pixel-intensity ^double pixel-intensity ^double pixel-intensity 1.0)
                (.setRGB im ix iy (c/argb-from-vector4 colour-result))))
            (if plane-intersection
              (let [u-tex (mget plane-intersection 0)
                    v-tex (mget plane-intersection 2)
                    pixel-intensity (checker-texture u-tex v-tex 2)]
                (.setValues colour-result ^double pixel-intensity ^double pixel-intensity ^double pixel-intensity 1.0)
                (.setRGB im ix iy (c/argb-from-vector4 colour-result)))
              (let [pixel-intensity 0.0]
                (.setValues colour-result pixel-intensity pixel-intensity pixel-intensity 1.0)
                (.setRGB im ix iy (c/argb-from-vector4 colour-result))))))))
    im))

(time (image/show (render-reflective-sphere) :title "Isn't it beautiful?"))
;(time (image/show (render-sphere) :title "Isn't it beautiful?"))


