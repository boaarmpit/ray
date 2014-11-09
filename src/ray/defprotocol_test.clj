(ns ray.defprotocol-test
  (:require [mikera.vectorz.core :as v]))

(defrecord Ray [P0 P1])

(defprotocol SceneObject
  (intersect [this ray]))

(defrecord Sphere [center radius]
  SceneObject
  (intersect [this ray]
    (print "intersecting ray with sphere\n")
    ))

(defrecord Plane [point normal]
  SceneObject
  (intersect [this ray]
    (print "intersecting ray with plane\n")
    ))


(let [ray (Ray.  (v/vec3 [0 0 0]) (v/vec3 [0 0 1]))
      my-sphere (Sphere. [0 0 0] 0.5)]
  (intersect my-sphere ray))

(let [ray (Ray.  (v/vec3 [0 0 0]) (v/vec3 [0 0 1]))
      my-plane (Plane. [0 0 0] [0 1 0])]
  (intersect my-plane ray))




(let [my-ray (Ray. (v/vec3 [0 0 0]) (v/vec3 [0 0 1]))]
  (:P0 my-ray))


(->Sphere [0 0 0] 0.5)
(Sphere. [0 0 0] 0.5)





(intersect (Sphere. (v/vec3 [0 0 1]) 1) (Ray. (v/vec3 [0 0 0]) (v/vec3 [0 1 0])))


