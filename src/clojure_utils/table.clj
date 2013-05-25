(ns clojure-utils.table
  ^{:author "Jake Piccolo"
    :doc "This mini-library is used to convert a standard CSV file into a sequence of hashmaps, making an easily query-able data structure for static data.

      The first row of the CSV should be the column headers, of the form
        header1,header2,header3

      Each hash-map in the resulting sequence will look like
        {:header1 val1, :header2 val2, :header3 val3}
      where val1, val2, val3 are the corresponding values in a given row.

      Use deftable to define a new table."}
  (:use [clojure-csv.core :only [parse-csv]]
        [clojure.java.io :only [reader resource]]))


(defn filename->csv
  "Convert the given CSV filename to a 2D matrix."
  [filename]
  (-> filename
      resource
      reader
      parse-csv))

(defn filename->table
  "Convert the given filename to a sequence of hash-maps."
  [filename]
  (let [[headers & body] (filename->csv filename)
        header-keys (map keyword headers)]
    (map (partial zipmap header-keys) body)))

(defmacro deftable
  "Returns a sequence of hash-maps.
   Example usage:
     (deftable table \"test.csv\")
   assuming there is a file resources/test.csv"
  [table-name file-name]
  `(def ~table-name (filename->table ~file-name)))

(def get-first (comp first filter))

(defn headers [table]
  (keys (first table)))

(defn matches-row-with? [matchmap f row]
  (every?
    #(f (% matchmap) (% row))
    (keys matchmap)))

(defn map-vals
  "Maps f to every val in a hash-map, storing the
   results in a new hash-map."
  [f hmap]
  (into {} (for [[k v] hmap] [k (f v)])))

(defn all-equal?
  "Example usage:
    (filter
      (all-equal? {:name \"Jordan\" :age \"18\"})
      table)

   The above code will find all rows of table with a name column of
   \"John\" and an age of 30."
  [matchmap]
  (partial matches-row-with? (map-vals str matchmap) =))

(defn all-re?
  "Like all-equal?, but expects the values of matchmap to be regexes."
  [matchmap]
  (partial matches-row-with? matchmap re-find))

(defn extract [kword row]
  {:pre [(not (nil? row))]}
  (read-string (kword row)))
