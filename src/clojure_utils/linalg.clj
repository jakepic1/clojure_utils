(ns clojure-utils.linalg
  "Assorted linear algebra functions.")


;; Scaling

(defn scale
  "Vector * scalar."
  [v x]
  (mapv (partial * x) v))

(defn A*x
  "Matrix * scalar."
  [A x]
  (mapv #(scale % x) A))


;; Vector operations

(defn dot
  "Compute the dot product of u and v."
  [u v]
  (apply + (map * u v)))

(defn norm
  "Norm (length) of vector v."
  [v]
  (Math/sqrt (dot v v)))

(defn dimension?
  "True if vector v is of dimension d."
  [d v]
  (= d (count v)))

(defn cross
  "Compute the cross product of u and v.
  The cross product is only defined for 3-dimensional vectors."
  [u v]
  {:pre [(every? (partial dimension? 3) [u v])]}
  (let [[[u1 u2 u3] [v1 v2 v3]] [u v]]
    [(- (* u2 v3) (* u3 v2))
     (- (* u3 v1) (* u1 v3))
     (- (* u1 v2) (* u2 v1))]))


;; Matrix functions

(defn A+B
  "Matrix addition."
  [A B]
  (mapv (partial mapv +) A B))

(defn A*v
  "Matrix * vector."
  [A v]
  (mapv #(apply + (mapv * % v)) A))

(defn transpose [A]
  (into [] (apply map vector A)))

(defn A*B [A B]
  "Matrix multiplication."
  (transpose (map (partial A*v A) (transpose B))))

(defn size
  "Get the size of a matrix or vector as a vector [rows, cols]."
  [[r1 :as A]]
  (if (coll? r1)
    [(count A) (count r1)]
    [1 (count A)]))

(defn pretty-print [A]
  (doseq [row A]
    (->> (interpose " " row) (apply str) println)))
