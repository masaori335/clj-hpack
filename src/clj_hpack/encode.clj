(ns clj-hpack.encode
  (:use [clj-hpack.core :refer :all]
        [clj-hpack.header-table :refer :all]))

;;; Encoder

(def cursor (atom 0))

(defn encode-integer-representation
  "6.1 Integer Representation"
  [i n]
  (if (< i (p2n-1 n))
    [i]
    (loop [i (- i (p2n-1 n)) result [(p2n-1 n)]]
      (if (< i 128)
        (conj result i)
        (recur (quot i 128) (conj result (+ (bit-and i 127) 128)))))))

(defn encode
  "TODO: Encode header"
  [table buf]
  [0])
