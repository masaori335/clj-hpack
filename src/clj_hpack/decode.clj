(ns clj-hpack.decode
  (:use [clj-hpack.core :refer :all])
  (:require [clj-hpack.header-table :as header-table]))

;;; Decoder

(def cursor (atom 0))

(defn current-octet
  [buf]
  (get buf @cursor))

(defn next-octet
  [buf]
  (get buf (inc @cursor)))

(defn next-octet!
  [buf]
  (get buf (swap! cursor inc)))

(defn decode-integer-representation
  "6.1 Integer Representation"
  [buf n]
  (let [b (get buf @cursor)
        i (bit-and (p2n-1 n) b)]
    (if (< i (p2n-1 n))
      (do
        (next-octet! buf)
        i)
      (loop [b (next-octet! buf) i (p2n-1 n) m 0]
        (if (zero? (bit-and b 128))
          (+ i (* (bit-and b 127) (bit-shift-left 1 m)))
          (recur (next-octet! buf) (+ i (* (bit-and b 127) (bit-shift-left 1 m))) (+ m 7)))))))

(defn decode-huffman-string
  "TODO: decode huffman encoded string"
  [string-data]
  "")

(defn decode-raw-string
  "TODO: Decode raw string"
  [buf length]
  (let [data (subvec buf @cursor (swap! cursor #(+ % length)))]
    (if debug? (println "[decode-raw-string] data:" data))
    (clojure.string/join (map char data))))

(defn decode-string-literal-representation
  "6.2 String Literal Representation"
  [buf]
  (let [huffman? (bit-test (current-octet buf) 7)
        string-length (decode-integer-representation buf 7)]
    (if debug? (println "[decode-string-literal-representation] string-length" string-length))
    (if huffman?
      (decode-huffman-string buf string-length)
      (decode-raw-string buf string-length))))

;; 7. Binary Format

(defn indexed-header-field-representation
  "7.1 Indexed Header Field Representation"
  [table buf]
  (let [index (decode-integer-representation buf 7)]
    (header-table/record table index)))

(defn new-name
  "New Name"
  [buf]
  (if debug? (println "[new-name]"))
  (let [name (decode-string-literal-representation buf)
        value (decode-string-literal-representation buf)
        header [name value]]
    header))

(defn indexed-name
  "Indexed Name"
  [table buf prefix-bit]
  (if debug? (println "[indexed-name]"))
  (let [index (decode-integer-representation buf prefix-bit)
        indexed-header (header-table/record table index)
        value (decode-string-literal-representation buf)
        header [(first indexed-header) value]]
      header))

(defn literal-header-field-with-incremental-indexing
  "7.2.1 Literal Header Field with Incremental Indexing"
  [table buf]
  (if debug? (println "[literal-header-field-with-incremental-indexing]"))
  (if-not (zero? (bit-and (current-octet buf) 2r00111111))
    (let [header (indexed-name table buf 6)]
      (header-table/add! table header)
      header)
    (do
      (next-octet! buf)
      (let [header (new-name buf)]
        (header-table/add! table header)
        header))))

(defn literal-header-field-without-indexing
  "7.2.2 Literal Header Field without Indexing"
  [table buf]
  (if debug? (println "[literal-header-field-without-indexing]"))
  (if-not (zero? (bit-and (current-octet buf) 2r00001111))
    (indexed-name table buf 4)
    (do
      (next-octet! buf)
      (new-name buf))))

(defn literal-header-field-never-indexed
  "7.2.3 Literal Header Field never Indexed"
  [table buf]
  (if debug? (println "[literal-header-field-never-indexed]"))
  (if-not (zero? (bit-and (current-octet buf) 2r00001111))
    (indexed-name table buf 4)
    (do
      (next-octet! buf)
      (new-name buf))))

(defn header-table-size-update
  "7.3 Header Table Size Update"
  [buf]
  0)

(defn decode-a-header!
  [table buf]
  (let [head (current-octet buf)]
    (cond
     (bit-test head 7) (indexed-header-field-representation table buf)
     (bit-test head 6) (literal-header-field-with-incremental-indexing table buf)
     (= (bit-and head 2r11110000) 2r00000000) (literal-header-field-without-indexing table buf)
     (= (bit-and head 2r11110000) 2r00010000) (literal-header-field-never-indexed table buf)
     (= (bit-and head 2r11110000) 2r00100000) (header-table-size-update table buf)
     :else "error")))

(defn decode!
  "Decode header data.
   The 'buf' arg should be binary array
   This function have a side effect to update header-table
   TODO: decode from string
   TODO: header-table object"
  [table buf]
  (reset! cursor 0)

  (if debug? (println "[decode] table:" table))

  (loop [result '[]]
    (if (nil? (current-octet buf))
      (do
        (if debug? (println "[decode] result:" result))
        result)
      (recur (conj result (decode-a-header! table buf))))))
