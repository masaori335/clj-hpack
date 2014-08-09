(ns clj-hpack.header-table
  (:use [clj-hpack.core :refer :all]
        [clj-hpack.static-table :only (static-table)]))

(defprotocol IndexedTableProtocol
  "Access to Header Table"
  (ref-header [this index])
  (add-header! [this header])
  (ref-table [this]))

;; "TODO: Control header-table size
;;        http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#section-3.3.3"
(deftype IndexedTable [^:volatile-mutable header-table]
  IndexedTableProtocol
  (ref-header [this index]
    (if debug? (println "[ref-header] index:" index))
    (if (<= index 61)
      (nth static-table index)
      (nth header-table (- index 62))))

  (add-header! [this header]
    (if debug? (println "[add-header!] header-table" header-table))
    (set! header-table (cons header header-table))
    (if debug? (println "[add-header!] header-table" header-table)))

  (ref-table [this] header-table))
