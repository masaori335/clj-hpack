;; 3.3.  Indexing Tables
;;
;;   http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#section-3.3
;;
;;   <----------  Index Address Space ---------->
;;   <-- Static  Table -->  <-- Header  Table -->
;;   +---+-----------+---+  +---+-----------+---+
;;   | 1 |    ...    | s |  |s+1|    ...    |s+k|
;;   +---+-----------+---+  +---+-----------+---+
;;                          ^                   |
;;                          |                   V
;;                          Insertion Point      Dropping Point
;;


(ns clj-hpack.header-table
  (:use [clj-hpack.core :refer :all]
        [clj-hpack.static-table :only (static-table)]))

(defprotocol IHeaderTable
  "Access to Header Table"
  (record [this index])
  (add! [this header])
  (lookup [this header])
  (lookup-key [this key]))

;; "TODO: Control header-table size
;;        http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#section-3.3.3"
(deftype HeaderTable [^:volatile-mutable header-table]
  IHeaderTable
  (record [this index]
    (if debug? (println "[ref-header] index:" index))
    (if (<= index 61)
      (nth static-table index)
      (nth header-table (- index 62))))

  (add! [this header]
    (if debug? (println "[add-header!] header-table" header-table))
    (set! header-table (cons header header-table))
    (if debug? (println "[add-header!] header-table" header-table)))

  (lookup [this header]
    (let [index (.indexOf static-table header)]
      (if debug? (println "[lookup] header" header ", index in static-table" index))
      (if (neg? index)
        (let [index (.indexOf header-table header)]
          (if debug? (println "[lookup] header" header ", index in header-table" index))
          (if (neg? index)
            index
            (+ index 62)))
        index)))

  (lookup-key [this key]
    (let [index (.indexOf static-table (first (filter #(= (first %) key) static-table)))]
      (if (neg? index)
        (.indexOf header-table (first (filter #(= (first %) key) header-table)))
        index))))
