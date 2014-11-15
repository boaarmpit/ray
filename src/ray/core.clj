(ns ray.core
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:require [clojure.math.numeric-tower :as math])
  (:require [mikera.image.core :as image])
  (:require [ray.colours :as c])
  (:require [mikera.vectorz.core :as v])
  (:require [mikera.vectorz.matrix :as m])
  (:import [java.awt.image BufferedImage])
  (:import [mikera.vectorz Vector3 Vector4 AVector Vectorz]))
(set-current-implementation :vectorz)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
(def pi (* 4 (Math/atan 1)))

;;image horizontal and vertical resolution
(def res-x 500)
(def res-y 500)
(defn new-image
  "Creates a new blank image"
  ([w h]
   (mikera.gui.ImageUtils/newImage (int w) (int h))))

(defprotocol SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction]}])
  (surface-reflectivity [this]))

(defrecord Lambertian [lambert-colour]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction] :or {}}]
    (let [lambert-component (- (v/dot normal light-direction))]
      (* lambert-colour lambert-component)))
  (surface-reflectivity [this]))

(defrecord Lambertian-mono []
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction] :or {}}]
    (- (v/dot normal light-direction)))
  (surface-reflectivity [this]))

(defrecord Phong [phong-colour phong-exponent]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction] :or {}}]
    (let [light-reflection-direction (+ (* -2 (v/dot light-direction normal) normal) light-direction)
          phong-dot-light-eye (v/dot light-reflection-direction eye-direction)
          phong-component (if (pos? phong-dot-light-eye) (math/expt phong-dot-light-eye phong-exponent) 0)]
      (* phong-colour phong-component)))
  (surface-reflectivity [this]))

(defrecord LambertPhong [lambert-weight lambert-colour phong-weigh phong-colour phong-exponent]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction] :or {}}]
    (let [light-reflection-direction (+ (* -2 (v/dot light-direction normal) normal) light-direction)
          phong-dot-light-eye (v/dot light-reflection-direction eye-direction)
          phong-component (if (pos? phong-dot-light-eye) (math/expt phong-dot-light-eye phong-exponent) 0)
          lambert-component (- (v/dot normal light-direction))]
      (+ (* phong-weigh phong-colour phong-component)
         (* lambert-weight lambert-colour lambert-component)
         )))

  (surface-reflectivity [this]))
;checkerboard texture: returns 1 or 0 for x,y coordinates on checker-spacing size grid
(defn checker-texture [x y checker-spacing]
  (let [xs (math/round (/ (mod x checker-spacing) checker-spacing))
        ys (math/round (/ (mod y checker-spacing) checker-spacing))]
    (* 0.7 (mod (+ xs ys) 2.0))))

;define object types (spheres and planes)
(defprotocol SceneObject
  (intersect [this ray] [this ray camera-origin]))

(defrecord Sphere [center radius surface]
  SceneObject
  (intersect [this ray]
    ;(print "intersecting ray with sphere\n")
    (let [P0 (:ray-origin ray)
          P1 (:ray-direction ray)
          C center
          r radius]
      (v/normalise! P1)
      (let [a (v/dot P1 P1)
            b (* 2 (v/dot P1 (- P0 C)))
            c (- (v/dot (- P0 C) (- P0 C)) (* r r))
            det (- (* b b) (* 4 a c))
            hit (pos? det)
            t (if hit (/ (- 0 b (math/sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
            intersection (if hit (+ P0 (* t P1)))
            normal (if intersection (v/normalise (- intersection C)))
            distance (if intersection (v/distance P0 intersection))]
        {:scene-object this :sphere-intersection intersection :normal normal :distance distance}))))




;define camera objects
(defrecord Camera [camera-location camera-direction camera-x camera-y])

;define rays
(defrecord Ray [ray-origin ray-direction])
;and ray-related functions
(defn camera-ray [^Camera camera x y]
  (let [camera-direction (:camera-direction camera)
        camera-x (:camera-x camera)
        camera-y (:camera-y camera)]
    (+ (* 2 (- (/ (+ (rand) (double x)) res-x) 0.5) camera-x) (* -2 (- (/ (+ (rand) (double y)) res-y) 0.5) camera-y) camera-direction)))


(defrecord Plane [point u-axis v-axis]
  SceneObject
  (intersect [this ray]
    (let [P1 (:ray-direction ray)
          M (array [u-axis v-axis (- P1)])
          det (m/determinant M)]
      (if-not (zero? det) 1 0))))


;testing plane intersection
(do
  (def my-plane (map->Plane {:point  (v/vec [0 0 0])
                             :u-axis (v/vec [1 0 1])
                             :v-axis (v/vec [0 0 1])}))

  (def my-ray (map->Ray {:ray-origin    (v/vec [0 0 0])
                         :ray-direction (v/vec [5 0 2])}))

  (intersect my-plane my-ray))

(defn closest
  ([object0] object0)
  ([object0 object1]
   (let [distance0 (:distance object0)
         distance1 (:distance object1)]
     (if distance0 (if distance1 (if (< distance0 distance1) object0 object1) object0) (if distance1 object1 nil))
     )))

(defn render-spheres []
  (let [colour-result (v/vec4 [0 0 0 1])
        ^Vector3 light-direction (v/normalise (v/vec3 [-1 -1 2]))
        my-camera (map->Camera {:camera-location  ^Vector3 (v/vec3 [0 0.5 -1])
                                :camera-direction ^Vector3 (v/vec3 [0 0 1])
                                :camera-x         ^Vector3 (v/vec3 [1 0 0])
                                :camera-y         ^Vector3 (v/vec3 [0 1 0])})

        sphere-surface0 (map->LambertPhong {:lambert-weight 0.8 :lambert-colour (v/vec [1 0 0])
                                            :phong-weigh    1 :phong-colour (v/vec [1 1 1])
                                            :phong-exponent 50})
        sphere-surface1 (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec [1 0 1])
                                            :phong-weigh    0.5 :phong-colour (v/vec [1 0 1])
                                            :phong-exponent 5})
        sphere-surface2 (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec [0 1 0])
                                            :phong-weigh    0.2 :phong-colour (v/vec [0 1 1])
                                            :phong-exponent 2})

        my-sphere0 (map->Sphere {:center ^Vector3 (v/vec3 [1 0 3]) :radius 1.2 :surface sphere-surface0})
        my-sphere1 (map->Sphere {:center ^Vector3 (v/vec3 [0 1.7 3]) :radius 1.2 :surface sphere-surface1})
        my-sphere2 (map->Sphere {:center ^Vector3 (v/vec3 [-1 0 3]) :radius 1.2 :surface sphere-surface2})

        scene-objects [my-sphere0 my-sphere1 my-sphere2]

        ^BufferedImage im (new-image res-x res-y)]

    (dotimes [ix res-x]
      (dotimes [iy res-y]
        (let [ray (Ray. (:camera-location my-camera) (camera-ray my-camera ix iy))
              rays (repeat (count scene-objects) ray)
              intersections (map intersect scene-objects rays)
              closest-intersection (reduce closest intersections)
              ]

          (if (:sphere-intersection closest-intersection)
            (let [
                  sphere-intersection (:sphere-intersection closest-intersection)
                  scene-object (:scene-object closest-intersection)
                  normal (:normal closest-intersection)
                  eye-direction (- (:ray-direction ray))
                  ^Vector3 pixel-colour (surface-colour (:surface scene-object) {:normal normal :light-direction light-direction :eye-direction eye-direction})
                  pixel-r (v/get pixel-colour 0)
                  pixel-g (v/get pixel-colour 1)
                  pixel-b (v/get pixel-colour 2)]
              ;MUTATING colour-result and im
              (.setValues colour-result pixel-r pixel-g pixel-b 1.0)
              (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))))
    im))

(time (image/show (render-spheres) :title "Isn't it beautiful?"))







