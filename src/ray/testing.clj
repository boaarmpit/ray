(ns ray.testing)

;(defn fibs
;  ([]
;    (fibs 1 1))
;  ([value0 value1]
;    (println value0)
;    (if (> value0 20)
;      (println "done")
;      (fibs  value1 (+ value0 value1)))))
;
;(defn factorial [x]
;  (if (< x 2)
;    1
;    (* x (factorial (- x 1)))
;    ))
;
;(loop [iteration 0 tmp 0]
;  (println (str "Iteration " iteration))
;  (if (> iteration 3)
;    (println "Goodbye!")
;    (recur (inc iteration) 0)))
;
;
;(loop [i0 1 i1 1]
;  (println i0)
;  (if (> i1 10)
;    (print "goodbye")
;    (recur i1 (+ i0 i1))))


(defn incr [input-list]
  (loop [to-process input-list
         processed []]
    (if to-process
      (let [[item & remaining] to-process
            processed (conj processed item)]
        (recur remaining (conj processed (inc item))))
      processed)))





(defn incr2 [to-process]
  (reduce (fn [processed part]
            (print processed part "\n")
            (let [processed (conj processed part)]
              (conj processed (inc part))))
          []
          to-process))

(print (incr2 [2 4 6]))

