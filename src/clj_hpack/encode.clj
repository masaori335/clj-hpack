(ns clj-hpack.encode
  (:use [clj-hpack.core :refer :all]
        [clj-hpack.header-table :refer :all]))

;;; Encoder

(def cursor (atom 0))

(defn integer-representation
  "6.1 Integer Representation"
  [i n]
  (if (< i (p2n-1 n))
    [i]
    (loop [i (- i (p2n-1 n)) result '((p2n-1 n))]
      (if (< i 128)
        (conj result i)
        (recur (quot i 128) (cons result (+ (bit-and i 127) 128)))))))

(defn string-literal-representation
  "6.2 String Literal Representation"
  [string huffman?]
  (if huffman?
    [0]
    (let [head (integer-representation (count string) 7)
          data (map byte (seq (char-array string)))]
      (concat head data))))

(defn set-prefix-bit
  [data prefix]
  (cons (bit-set (first data) prefix) (rest data)))

(defn indexed-header-field-representation
  "7.1 Indexed Header Field Representation"
  [table header]
  (let [data (integer-representation (lookup table header) 7)]
    (set-prefix-bit data 7)))

(defn literal-header-field-with-icremental-indexing
  "7.2.1.  Literal Header Field with Incremental Indexing (new name)"
  [table header]
  (add! table header)
  (let [index (lookup-key table key)]
    (if (> index 0)
      (concat (set-prefix-bit (integer-representation index 5) 6)
              (string-literal-representation (last header) false))
      (concat '(2r01000000)
              (string-literal-representation (first header) false)
              (string-literal-representation (last header) false)))))

(defn literal-header-field-without-indexing
  "7.2.2.  Literal Header Field without Indexing"
  [table header]
  (let [index (lookup-key table (first header))]
    (if (> index 0)
      (concat (integer-representation index 4)
              (string-literal-representation (last header) false))
      (concat '(2r00000000)
              (string-literal-representation (first header) false)
              (string-literal-representation (last header) false)))))

(defn literal-header-field-never-indexed
  "7.2.3.  Literal Header Field never Indexed"
  [table header]
  (let [index (lookup-key table key)]
    (if (> index 0)
      (concat (set-prefix-bit (integer-representation index 4) 5)
              (string-literal-representation (last header) false))
      (concat '(2r00010000)
              (string-literal-representation (first header) false)
              (string-literal-representation (last header) false)))))

(defn encode-a-header!
  [table header & {:keys [sensitive] :or {sensitive false}}]
  (if sensitive
    (literal-header-field-never-indexed table header)
    (let [key (first header)
          value (last header)
          index (lookup table header)]
      (if (> index 0)
        (indexed-header-field-representation index)
        (literal-header-field-with-icremental-indexing table header)))))

(defn encode!
  "TODO: Encode header"
  [table headers]
  (loop [headers headers table table result '[]]
    (if (nil? headers)
      result
      (recur (rest headers) table (conj result (encode-a-header! table (first headers)))))))
