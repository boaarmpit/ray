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
(defrecord Lambertian []
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction]}]
    (let [lambert (- (v/dot normal light-direction))]
      (if (pos? lambert) lambert 0)))
  (surface-reflectivity [this]))
(defrecord Phong [phong-exponent]
  SurfaceObject
  (surface-colour [this {:keys [normal light-direction eye-direction]}]
    (let [light-reflection-direction (+ (* -2 (v/dot light-direction normal) normal) light-direction)
          dot-light-eye (v/dot light-reflection-direction eye-direction)]
      (if (pos? dot-light-eye)
        (math/expt dot-light-eye phong-exponent)
        0)))
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

;define object types (spheres and planes)
(defprotocol SceneObject
  (intersect [this ray] [this ray camera-origin]))
(defrecord Sphere [center radius surface]
  SceneObject
  (intersect
    [this ray]
    ;(print "intersecting ray with sphere\n")
    (let [P0 (:P0 ray)
          P1 (:P1 ray)
          C center
          r radius]
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
        {:sphere-intersection intersection :normal normal})))
  (intersect
    [this ray ray-origin]
    ;(print "intersecting ray with sphere\n")
    (let [P0 (:P0 ray)
          P1 (:P1 ray)
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
            distance (if intersection (v/distance ray-origin intersection))]
        {:scene-object this :sphere-intersection intersection :normal normal :distance distance}))))
(defrecord Plane [point normal]
  SceneObject
  (intersect [this ray]
    (print "intersecting ray with plane\n")))

;checkerboard texture: returns 1 or 0 for x,y coordinates on checker-spacing size grid
(defn checker-texture [x y checker-spacing]
  (let [xs (math/round (/ (mod x checker-spacing) checker-spacing))
        ys (math/round (/ (mod y checker-spacing) checker-spacing))]
    (* 0.7 (mod (+ xs ys) 2.0))))

;define camera objects
(defrecord Camera [camera-location camera-direction camera-x camera-y])

;define rays
(defrecord Ray [P0 P1])
;and ray-related functions
(defn camera-ray [^Camera camera x y]
  (let [camera-direction (:camera-direction camera)
        camera-x (:camera-x camera)
        camera-y (:camera-y camera)]
    (+ (* 2 (- (/ (+ (rand) (double x)) res-x) 0.5) camera-x) (* -2 (- (/ (+ (rand) (double y)) res-y) 0.5) camera-y) camera-direction)))

(comment
  (defn render-sphere2 []
    (let [colour-result (v/vec4 [0 0 0 1])

          ^Vector3 light-direction (v/normalise (v/vec3 [-1 -1 2]))
          my-camera (map->Camera {:camera-location  ^Vector3 (v/vec3 [0 1 0])
                                  :camera-direction ^Vector3 (v/vec3 [0 0 1])
                                  :camera-x         ^Vector3 (v/vec3 [1 0 0])
                                  :camera-y         ^Vector3 (v/vec3 [0 1 0])})


          sphere-surface (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec [1 0 1])
                                             :phong-weigh    0.5 :phong-colour (v/vec [1 0 1])
                                             :phong-exponent 5})

          my-sphere (map->Sphere {:center ^Vector3 (v/vec3 [0 0.5 3]) :radius 1.5 :surface sphere-surface})


          ^BufferedImage im (new-image res-x res-y)]

      (dotimes [ix res-x]
        (dotimes [iy res-y]
          (let [ray (Ray. (:camera-location my-camera) (camera-ray my-camera ix iy))
                {:keys [sphere-intersection normal]} (intersect my-sphere ray)]
            (if sphere-intersection
              (let [
                    eye-direction (- (:P1 ray))
                    ^Vector3 pixel-colour (surface-colour (:surface my-sphere) {:normal normal :light-direction light-direction :eye-direction eye-direction})
                    pixel-r (v/get pixel-colour 0)
                    pixel-g (v/get pixel-colour 1)
                    pixel-b (v/get pixel-colour 2)]
                ;MUTATING colour-result and im
                (.setValues colour-result pixel-r pixel-g pixel-b 1.0)
                (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))))
      im)))

(defn closest [object0 object1]
  (let [distance0 (:distance object0)
        distance1 (:distance object1)]
    (if distance0 (if distance1 (if (< distance0 distance1) object0 object1) object0) (if distance1 object1 nil))
    ))


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
              rays (vec (repeat (count scene-objects) ray))
              ray-origins (vec (repeat (count scene-objects) (:camera-location my-camera)))
              intersections (map intersect scene-objects rays ray-origins)
              closest-intersection (reduce closest intersections)
              ]

          (if (:sphere-intersection closest-intersection)
            (let [
                  sphere-intersection (:sphere-intersection closest-intersection)
                  scene-object (:scene-object closest-intersection)
                  normal (:normal closest-intersection)
                  eye-direction (- (:P1 ray))
                  ^Vector3 pixel-colour (surface-colour (:surface scene-object) {:normal normal :light-direction light-direction :eye-direction eye-direction})
                  pixel-r (v/get pixel-colour 0)
                  pixel-g (v/get pixel-colour 1)
                  pixel-b (v/get pixel-colour 2)]
              ;MUTATING colour-result and im
              (.setValues colour-result pixel-r pixel-g pixel-b 1.0)
              (.setRGB im ix iy (c/argb-from-vector4 colour-result)))))))
    im))


(time (image/show (render-spheres) :title "Isn't it beautiful?"))


:old
(comment


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

  ;(time (image/show (render-reflective-sphere) :title "Isn't it beautiful?"))


  (defn camera-ray2
    "x and y are coordinates from 0 to 1, starting at the top left of the image"
    [ix iy camera-direction camera-x camera-y]
    (let [temp1 (v/add-multiple camera-direction camera-x (* 2 (- (/ (double ix) res-x) 0.5)))
          temp2 (v/add-multiple temp1 camera-y (* -2 (- (/ (double iy) res-y) 0.5)))
          temp3 (v/normalise temp2)]
      temp3))

  ;(defn camera-ray
  ;  "x and y are pixel coordinates starting at the top left of the image"
  ;  [ix iy camera-direction camera-x camera-y]
  ;  (+ (* 2 (- (/ (+ (rand) (double ix)) res-x) 0.5) camera-x) (* -2 (- (/ (+ (rand) (double iy)) res-y) 0.5) camera-y) camera-direction))

  )



