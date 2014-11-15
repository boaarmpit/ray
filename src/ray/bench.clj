(ns ray.bench)



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
    im))
(time (image/show (render-sphere2) :title "Isn't it beautiful?"))


;for benchmarking
(comment
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
          (let [{:keys [sphere-intersection normal]} (ray-sphere camera-location dir sphere-origin sphere-radius)
                ]
            (if sphere-intersection
              (let [pixel-intensity (+ (/ (math/expt (- (v/mget normal 2)) 30) 1.5) (/ (- (v/mget normal 2)) 3))]
                (.setValues colour-result pixel-intensity pixel-intensity pixel-intensity 1.0)
                (.setRGB im ix iy (c/argb-from-vector4 colour-result))))
            )))
      im))
  (defn render-sphere-new []
    (let [colour-result (v/vec4 [0 0 0 1])
          ^Vector3 light-direction (v/normalise (v/vec3 [-1 -1 2]))
          my-camera (map->Camera {:camera-location  ^Vector3 (v/vec3 [0 1 0])
                                  :camera-direction ^Vector3 (v/vec3 [0 0 1])
                                  :camera-x         ^Vector3 (v/vec3 [1 0 0])
                                  :camera-y         ^Vector3 (v/vec3 [0 1 0])})

          sphere-surface0 (map->Phong {:phong-exponent 30 :phong-colour (v/vec [1 0 1])})
          sphere-surface1 (map->Lambertian {:lambert-colour (v/vec [1 0 1])})
          sphere-surface2 (->Lambertian-mono)
          sphere-surface3 (map->LambertPhong {:lambert-weight 0.5 :lambert-colour (v/vec [0 1 0])
                                              :phong-weigh    0.2 :phong-colour (v/vec [0 1 1])
                                              :phong-exponent 2})

          my-sphere0 (map->Sphere {:center ^Vector3 (v/vec3 [0 0.5 3]) :radius 1.5 :surface sphere-surface3})
          scene-objects [my-sphere0]
          ;MUTABLES
          ^Vector3 dir (v/vec3 [0 0 0])
          ^BufferedImage im (new-image res-x res-y)]

      (dotimes [ix res-x]
        (dotimes [iy res-y]
          (.set dir (:camera-direction my-camera))
          (v/add-multiple! dir (:camera-x my-camera) (* 2 (- (/ (double ix) res-x) 0.5)))
          (v/add-multiple! dir (:camera-y my-camera) (* -2 (- (/ (double iy) res-y) 0.5)))
          (v/normalise! dir)
          (let [ray (Ray. (:camera-location my-camera) dir)
                rays (vec (repeat (count scene-objects) ray))
                ;ray-origins (vec (repeat (count scene-objects) (:camera-location my-camera)))
                ;intersections (map intersect scene-objects rays ray-origins)
                ;closest-intersection (reduce closest intersections)
                closest-intersection (intersect my-sphere0 ray)
                ]

            (if (:sphere-intersection closest-intersection)
              (let [
                    sphere-intersection (:sphere-intersection closest-intersection)
                    scene-object my-sphere0                              ;(:scene-object closest-intersection)
                    normal (:normal closest-intersection)
                    eye-direction (- (:P1 ray))
                    ^Vector3 pixel-colour (surface-colour (:surface scene-object) {:normal normal :light-direction light-direction :eye-direction eye-direction})
                    pixel-r (v/get pixel-colour 0)
                    pixel-g (v/get pixel-colour 1)
                    pixel-b (v/get pixel-colour 2)
                    ]
                ;MUTATING colour-result and im
                (.setValues colour-result pixel-r pixel-g pixel-b 1.0)
                (.setRGB im ix iy (c/argb-from-vector4 colour-result))
                )
              )
            )))
      im))
  ;(time (dotimes [i 20] (image/show (render-sphere-new) :title "Isn't it beautiful?")))
  ;(time (dotimes [i 20] (image/show (render-sphere) :title "Isn't it beautiful?")))
  )


;old
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