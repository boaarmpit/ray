(ns ray.image-test
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators))
(require '[mikera.image.core :as image])
(require '[mikera.image.colours :as colours])
(set-current-implementation :vectorz)


;; create a new image
(def bi (image/new-image 3 3))

;; gets the pixels of the image, as an int array
(def pixels (image/get-pixels bi))

;; fill some random pixels with colours
(dotimes [i 9]
  (aset pixels i (colours/rand-colour)))

;; update the image with the newly changed pixel values
(image/set-pixels bi pixels)

;; view our new work of art
;; the zoom function will automatically interpolate the pixel values
(image/show bi :zoom 100 :title "Isn't it beautiful?")

(def A (matrix [[1 0 0] [0 1 0] [0 0 1]]))
(pm A)
;;image horizontal and vertical resolution
(def res-x 3)
(def res-y 3)

;; create a new image
(def bi (image/new-image res-x res-y))

;; gets the pixels of the image, as an int array
(def pixels (image/get-pixels bi))


(def A (identity-matrix res-x))
(pm A)

;; fill some random pixels with colours
(do

  (comment (dotimes [i (* res-x res-y)]
             (def x (mod i res-x))
             (def y (quot i res-x))
             (aset pixels i (colours/rgb (mod (/ i 333) 1) (mod (/ i 334) 1) (mod (/ i 335) 1)))))

  (dotimes [i 9] (aset pixels i (colours/rgb (nth (eseq A) i) 1 1)))

  ;; update the image with the newly changed pixel values
  (image/set-pixels bi pixels)

  ;; view our new work of art
  ;; the zoom function will automatically interpolate the pixel values
  (image/show bi :zoom (/ 100 res-x) :title "Isn't it beautiful?"))

