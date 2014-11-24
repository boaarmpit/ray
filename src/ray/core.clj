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
(defn new-image
  "Creates a new blank image"
  ([w h]
    (mikera.gui.ImageUtils/newImage (int w) (int h))))      ;setup

;define camera objects
(defrecord Camera [camera-location camera-direction camera-x camera-y])

;define rays
(defrecord Ray [ray-origin ray-direction])
;and ray-related functions
(defn camera-ray [^Camera camera res-x res-y x y]
  (let [camera-direction (:camera-direction camera)
        camera-x (:camera-x camera)
        camera-y (:camera-y camera)]
    ;(v/normalise (+ (* 2 (- (/ (double x) res-x) 0.5) camera-x) (* -2 (- (/ (double y) res-y) 0.5) camera-y) camera-direction))
    (v/normalise (+ (* 2 (- (/ (+ (rand) (double x)) res-x) 0.5) camera-x) (* -2 (- (/ (+ (rand) (double y)) res-y) 0.5) camera-y) camera-direction))
    ))

;define geometric object types (spheres and planes)
(defprotocol SceneObject
  (intersect [this ray] [this ray camera-origin])
  (reflect [this ray intersection normal]))

(defrecord Sphere [center radius surface]
  SceneObject
  (intersect [this ray]
    ;(print "intersecting ray with sphere\n")
    (let [P0 (:ray-origin ray)
          P1 (:ray-direction ray)
          C center
          r radius]
      ;(v/normalise! P1) normalized at definition in camera-ray
      (let [a (v/dot P1 P1)
            b (* 2 (v/dot P1 (- P0 C)))
            c (- (v/dot (- P0 C) (- P0 C)) (* r r))
            det (- (* b b) (* 4 a c))
            hit-front-back (pos? det)
            t (if hit-front-back (/ (- 0 b (math/sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
            hit (if hit-front-back (pos? t))
            intersection (if hit (+ P0 (* t P1)))
            normal (if intersection (v/normalise (- intersection C)))
            distance (if hit t)
            ;distance (if intersection (v/distance P0 intersection))
            ]
        {:scene-object this :intersection intersection :normal normal :distance distance})))
  (reflect [this ray intersection normal]
    (map->Ray {:ray-origin (+ intersection (* 0.000001 normal)) :ray-direction (- (:ray-direction ray) (* 2 (dot (:ray-direction ray) normal) normal))})))
(defrecord Plane [plane-point u-axis v-axis normal surface]
  SceneObject
  (intersect [this ray]
    (let [ray-origin (:ray-origin ray)
          ray-direction (:ray-direction ray)]
      (if-not (zero? (dot ray-direction normal))
        (do
          (let [
                M (set-column (set-column (set-column (m/new-matrix 3 3) 0 u-axis) 1 v-axis) 2 (- ray-direction))
                M-inv (m/inverse M)
                b (mmul M-inv (- ray-origin plane-point))
                u-coord (v/get b 0)
                v-coord (v/get b 1)
                t (v/get b 2)
                hit (pos? t)
                intersection (if hit (+ ray-origin (* t ray-direction)))
                distance (if hit t)]
            {:scene-object this :intersection intersection :normal normal :distance distance
             :u-coord      u-coord :v-coord v-coord})))))
  (reflect [this ray intersection normal]
    (map->Ray {:ray-origin (+ intersection (* 0.000001 normal)) :ray-direction (- (:ray-direction ray) (* 2 (dot (:ray-direction ray) normal) normal))})))



(defprotocol TextureObject
  (texture [this] [this {:keys [u v]}]))

(defrecord Checker [scale]
  TextureObject
  (texture [this {:keys [u v]}]
    (let [us (math/round (/ (mod u scale) scale))
          vs (math/round (/ (mod v scale) scale))]
      (* (array [1 1 1]) (mod (+ us vs) 2.0)))))

(defrecord SolidColour [colour]
  TextureObject
  (texture [this {:keys []}]
    (texture this))
  (texture [this]
    colour))



;define surface object types
(defprotocol SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction u-coord v-coord]}]))

(defrecord Lambertian [lambert-colour]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction u-coord v-coord] :or {}}]
    (let [lambert-coefficient (- (v/dot normal light-direction))]
      (* lambert-colour lambert-coefficient))))

(defrecord Lambertian2 [lambert-colour]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction u-coord v-coord] :or {}}]
    (let [lambert-coefficient (- (v/dot normal light-direction))]
      (* (texture lambert-colour {:u u-coord :v v-coord}) lambert-coefficient))))



(defrecord Phong [phong-colour phong-exponent]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction] :or {}}]
    (let [light-reflection-direction (+ (* -2 (v/dot light-direction normal) normal) light-direction)
          phong-dot-light-eye (v/dot light-reflection-direction eye-direction)
          phong-component (if (pos? phong-dot-light-eye) (math/expt phong-dot-light-eye phong-exponent) 0)]
      (* phong-colour phong-component))))
(defrecord LambertPhong [lambert-weight lambert-colour phong-weigh phong-colour phong-exponent reflectivity]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction]}]
    (let [light-reflection-direction (+ (* -2 (v/dot light-direction normal) normal) light-direction)
          phong-dot-light-eye (v/dot light-reflection-direction eye-direction)
          phong-coefficient (if (pos? phong-dot-light-eye) (math/expt phong-dot-light-eye phong-exponent) 0)
          lambert-coefficient (- (v/dot normal light-direction))]
      (+ (* phong-weigh phong-colour phong-coefficient)
         (* lambert-weight lambert-colour lambert-coefficient)
         ))))




;checkerboard texture: returns 1 or 0 for x,y coordinates on checker-spacing size grid
(defn checker-texture [x y checker-spacing]
  (let [xs (math/round (/ (mod x checker-spacing) checker-spacing))
        ys (math/round (/ (mod y checker-spacing) checker-spacing))]
    (* 0.7 (mod (+ xs ys) 2.0))))


;define core ray tracing functions
(defn closest "Finds the closest of a pair of objects.  Used in trace-ray"
  ([object0] object0)
  ([object0 object1]
    (let [distance0 (:distance object0)
          distance1 (:distance object1)]
      (if distance0 (if distance1 (if (< distance0 distance1) object0 object1) object0) (if distance1 object1 nil))
      )))

(defn trace-ray "Traces a ray reflection-depth times and returns a colour.  Returns black if no object found."
  [ray scene-objects light-direction reflection-depth]
  (let [rays (repeat (count scene-objects) ray)
        intersections (map intersect scene-objects rays)
        closest-intersection (reduce closest intersections)]
    (if (:intersection closest-intersection)
      (let [
            intersection (:intersection closest-intersection)
            u-coord (:u-coord closest-intersection)
            v-coord (:v-coord closest-intersection)
            scene-object (:scene-object closest-intersection)
            normal (:normal closest-intersection)
            eye-direction (- (:ray-direction ray))
            reflectivity (:reflectivity (:surface scene-object))
            reflected-ray (if (and reflectivity (not (zero? reflectivity))) (reflect scene-object ray intersection normal))
            pixel-colour (if (and (> reflection-depth 0) reflected-ray)
                           (+ (* reflectivity (:pixel-colour (trace-ray reflected-ray scene-objects light-direction (- reflection-depth 1))))
                              (surface-colour (:surface scene-object) {:normal normal :light-direction light-direction :eye-direction eye-direction :u-coord u-coord :v-coord v-coord}))
                           (surface-colour (:surface scene-object) {:normal normal :light-direction light-direction :eye-direction eye-direction :u-coord u-coord :v-coord v-coord})
                           )]
        {:pixel-colour pixel-colour :reflected-ray (reflect scene-object ray intersection normal)})
      {:pixel-colour (array [0 0 0])}                       ;array much faster than v/vec3
      )))


(defn render-3spheres "Example scene depicting 3 adjacent reflective spheres." [resolution iterations]
  (let [^BufferedImage im (new-image resolution resolution)
        colour-result (v/vec4 [0 0 0 1])

        light-direction (v/normalise (v/vec3 [0 0 2]))
        my-camera (map->Camera {:camera-location  (v/vec3 [0 0.5 1])
                                :camera-direction (v/vec3 [0 0 1])
                                :camera-x         (v/vec3 [1 0 0])
                                :camera-y         (v/vec3 [0 1 0])})

        sphere-surface0 (map->LambertPhong {:lambert-weight 0.8 :lambert-colour (v/vec3 [1 0 0])
                                            :phong-weigh    1 :phong-colour (v/vec3 [1 1 1])
                                            :phong-exponent 50 :reflectivity 0.8})
        sphere-surface1 (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec3 [1 0 1])
                                            :phong-weigh    0.5 :phong-colour (v/vec3 [1 0 1])
                                            :phong-exponent 5 :reflectivity 0.8})
        sphere-surface2 (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec3 [0 1 0])
                                            :phong-weigh    0.2 :phong-colour (v/vec3 [0 1 1])
                                            :phong-exponent 2 :reflectivity 0.8})

        my-sphere0 (map->Sphere {:center (v/vec3 [1 0 3]) :radius 1 :surface sphere-surface0})
        my-sphere1 (map->Sphere {:center (v/vec3 [0 (math/sqrt 3) 3]) :radius 1 :surface sphere-surface1})
        my-sphere2 (map->Sphere {:center (v/vec3 [-1 0 3]) :radius 1 :surface sphere-surface2})

        plane-surface (map->LambertPhong {:lambert-weight 0.1 :lambert-colour (v/vec3 [1 1 1])
                                          :phong-weigh    1 :phong-colour (v/vec3 [1 1 1])
                                          :phong-exponent 5
                                          :reflectivity   1})

        scene-objects [my-sphere0 my-sphere1 my-sphere2]]   ;setup scene
    (dotimes [ix resolution]
      (dotimes [iy resolution]
        (let [ray (Ray. (:camera-location my-camera) (camera-ray my-camera resolution resolution ix iy))]
          (let [pixel-colour (:pixel-colour (trace-ray ray scene-objects light-direction iterations))
                pixel-r (if pixel-colour (v/get pixel-colour 0) 0)
                pixel-g (if pixel-colour (v/get pixel-colour 1) 0)
                pixel-b (if pixel-colour (v/get pixel-colour 2) 0)]
            (.setValues colour-result pixel-r pixel-g pixel-b 1.0)
            (.setRGB im ix iy (c/argb-from-vector4 colour-result))))))
    im))

(defn render-spheres-and-plane "Example scene depicting 3 adjacent reflective spheres." [resolution iterations]
  (let [^BufferedImage im (new-image resolution resolution)
        colour-result (v/vec4 [0 0 0 1])

        light-direction (v/normalise (v/vec3 [0 0 2]))
        my-camera (map->Camera {:camera-location  (v/vec3 [0 (/ (math/sqrt 3) 2) 0])
                                :camera-direction (v/vec3 [0 0 1])
                                :camera-x         (v/vec3 [1 0 0])
                                :camera-y         (v/vec3 [0 1 0])})


        sphere-surface0 (map->LambertPhong {:lambert-weight 0.1 :lambert-colour (v/vec3 [1 1 1])
                                            :phong-weigh    0.9 :phong-colour (v/vec3 [1 0 0])
                                            :phong-exponent 5 :reflectivity 1})
        sphere-surface1 (map->LambertPhong {:lambert-weight 0.1 :lambert-colour (v/vec3 [1 1 1])
                                            :phong-weigh    0.9 :phong-colour (v/vec3 [0 1 0])
                                            :phong-exponent 5 :reflectivity 1})
        sphere-surface2 (map->LambertPhong {:lambert-weight 0.1 :lambert-colour (v/vec3 [1 1 1])
                                            :phong-weigh    0.9 :phong-colour (v/vec3 [0 0 1])
                                            :phong-exponent 5 :reflectivity 1})


        my-sphere0 (map->Sphere {:center (v/vec3 [1 0 3]) :radius 1 :surface sphere-surface0})
        my-sphere1 (map->Sphere {:center (v/vec3 [0 (math/sqrt 3) 3]) :radius 1 :surface sphere-surface1})
        my-sphere2 (map->Sphere {:center (v/vec3 [-1 0 3]) :radius 1 :surface sphere-surface2})

        plane-surface (map->LambertPhong {:lambert-weight 0 :lambert-colour (v/vec3 [1 1 1])
                                          :phong-weigh    0 :phong-colour (v/vec3 [1 1 1])
                                          :phong-exponent 5 :reflectivity 1})

        my-plane0 (map->Plane {:plane-point (v/vec3 [1 0 0])
                               :u-axis      (v/vec3 [0 1 0])
                               :v-axis      (v/vec3 [0 0 1])
                               :normal      (- (cross! (v/vec3 [0 1 0]) (v/vec3 [0 0 1])))
                               :surface     plane-surface})

        my-plane1 (map->Plane {:plane-point (v/vec3 [-1 0 0])
                               :u-axis      (v/vec3 [0 0 1])
                               :v-axis      (v/vec3 [0 1 0])
                               :normal      (- (cross! (v/vec3 [0 0 1]) (v/vec3 [0 1 0])))
                               :surface     plane-surface})

        my-plane2 (map->Plane {:plane-point (v/vec3 [0 0 0])
                               :u-axis      (v/vec3 [1 0 0])
                               :v-axis      (v/vec3 [0 0 1])
                               :normal      (- (cross! (v/vec3 [1 0 0]) (v/vec3 [0 0 1])))
                               :surface     plane-surface})

        my-plane3 (map->Plane {:plane-point (v/vec3 [0 (math/sqrt 3) 0])
                               :u-axis      (v/vec3 [0 0 1])
                               :v-axis      (v/vec3 [1 0 0])
                               :normal      (- (cross! (v/vec3 [0 0 1]) (v/vec3 [1 0 0])))
                               :surface     plane-surface})

        scene-objects [my-sphere0 my-sphere1 my-sphere2 my-plane0 my-plane1 my-plane2 my-plane3]] ;setup scene
    (dotimes [ix resolution]
      (dotimes [iy resolution]
        (let [ray (Ray. (:camera-location my-camera) (camera-ray my-camera resolution resolution ix iy))]
          (let [pixel-colour (:pixel-colour (trace-ray ray scene-objects light-direction iterations))
                pixel-r (if pixel-colour (v/get pixel-colour 0) 0)
                pixel-g (if pixel-colour (v/get pixel-colour 1) 0)
                pixel-b (if pixel-colour (v/get pixel-colour 2) 0)]
            (.setValues colour-result pixel-r pixel-g pixel-b 1.0)
            (.setRGB im ix iy (c/argb-from-vector4 colour-result))))))
    im))


(defn test-scene "Example scene depicting 3 adjacent reflective spheres." [resolution iterations]
  (let [^BufferedImage im (new-image resolution resolution)
        colour-result (v/vec4 [0 0 0 1])

        light-direction (v/normalise (v/vec3 [0 -2 2]))
        my-camera (map->Camera {:camera-location  (v/vec3 [0 0.5 1.5])
                                :camera-direction (v/vec3 [0 0 1])
                                :camera-x         (v/vec3 [1 0 0])
                                :camera-y         (v/vec3 [0 1 0])})


        sphere-surface0 (map->LambertPhong {:lambert-weight 0.1 :lambert-colour (v/vec3 [1 1 1])
                                            :phong-weigh    0.9 :phong-colour (v/vec3 [1 0 0])
                                            :phong-exponent 5 :reflectivity 0.5})
        sphere-surface1 (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec3 [1 0 1])
                                            :phong-weigh    0.5 :phong-colour (v/vec3 [1 0 1])
                                            :phong-exponent 5 :reflectivity 0.5})
        sphere-surface2 (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec3 [0 1 0])
                                            :phong-weigh    0.2 :phong-colour (v/vec3 [0 1 1])
                                            :phong-exponent 2 :reflectivity 0.5})


        my-sphere0 (map->Sphere {:center (v/vec3 [1 0 3]) :radius 1 :surface sphere-surface0})
        my-sphere1 (map->Sphere {:center (v/vec3 [0 (math/sqrt 3) 3]) :radius 1 :surface sphere-surface1})
        my-sphere2 (map->Sphere {:center (v/vec3 [-1 0 3]) :radius 1 :surface sphere-surface2})


        ;(def my-texture (map->Checker {:scale 10}))
        ;(texture my-texture {:u 1 :v 5})
        ;(def my-texture (map->SolidColour {:colour "red"}))
        ;(texture my-texture)

        plane-texture0 (map->SolidColour {:colour (v/vec3 [1 0.5 0.5])})
        plane-texture1 (map->Checker {:scale 1.5})

        plane-surface (map->Lambertian2 {:lambert-colour plane-texture1})

        my-plane0 (map->Plane {:plane-point (v/vec3 [0 -1 0])
                               :u-axis      (v/vec3 [1 0 0])
                               :v-axis      (v/vec3 [0 0 1])
                               :normal      (- (cross! (v/vec3 [1 0 0]) (v/vec3 [0 0 1])))
                               :surface     plane-surface})


        scene-objects [my-plane0 my-sphere0 my-sphere1 my-sphere2]
        ;camera-direction (v/vec3 [0 0 1])
        ;camera-x (v/vec3 [1 0 0])
        ;camera-y (v/vec3 [0 1 0])
        ;dir camera-direction
        ;res-x resolution
        ;res-y resolution

        ]                                                   ;setup scene
    (dotimes [ix resolution]
      (dotimes [iy resolution]
        ;(.set dir camera-direction)
        ;(v/add-multiple! dir camera-x (* 2 (- (/ (double ix) res-x) 0.5)))
        ;(v/add-multiple! dir camera-y (* -2 (- (/ (double iy) res-y) 0.5)))
        ;(v/normalise! dir)
        (let [ray (Ray. (:camera-location my-camera) (camera-ray my-camera resolution resolution ix iy))
              pixel-colour (:pixel-colour (trace-ray ray scene-objects light-direction iterations))
              pixel-r (if pixel-colour (v/get pixel-colour 0) 0)
              pixel-g (if pixel-colour (v/get pixel-colour 1) 0)
              pixel-b (if pixel-colour (v/get pixel-colour 2) 0)]
          (.setValues colour-result pixel-r pixel-g pixel-b 1.0)
          (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))
    im))

;(time (image/show (render-spheres-and-plane 300 1) :title "Isn't it beautiful?"))
;(time (image/show (render-3spheres 500 10) :title "Isn't it beautiful?"))
(time (image/show (test-scene 200 2) :title "Isn't it beautiful?"))



