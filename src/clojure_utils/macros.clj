(ns clojure-utils.macros)


(defmacro let-reduce
  "Like let, but reduces bound locals in bindings with f,
  and binds the result to resname.

  This:
    (let-reduce z
      * [x 2
         y 3]
      (inc z))

  Becomes this:
    (let [x 2
          y 3]
      (let [z (reduce * [x y])]
        (inc z)))"
  [resname f bindings & body]
  (let [factors (take-nth 2 bindings)]
    `(let ~bindings
       (let [~resname (reduce ~f [~@factors])]
         ~@body))))

(defmacro $->
  "Anaphoric thread macro, using $ as the insertion point.
    ($-> building-base-rates (map $ :class_group) (filter #(> 3 %) $))"
  ([x] x)
  ([x & more]
    `(let [~'$ ~x]
       ($-> ~@more))))
