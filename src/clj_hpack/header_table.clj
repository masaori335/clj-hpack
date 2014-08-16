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
  (add! [this header]))

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
    (if debug? (println "[add-header!] header-table" header-table))))