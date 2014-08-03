(ns clj-hpack.core)

(def debug? true)

(def static-table
  [[":authority"                   ""]
   [":method"                      "GET"]
   [":method"                      "POST"]
   [":path"                        "/"]
   [":path"                        "/index.html"]
   [":scheme"                      "http"]
   [":scheme"                      "https"]
   [":status"                      "200"]
   [":status"                      "204"]
   [":status"                      "206"]
   [":status"                      "304"]
   [":status"                      "400"]
   [":status"                      "404"]
   [":status"                      "500"]
   ["accept-charset"               ""]
   ["accept-encoding"              "gzip, deflate"]
   ["accept-language"              ""]
   ["accept-ranges"                ""]
   ["accept"                       ""]
   ["access-control-allow-origin"  ""]
   ["age"                          ""]
   ["allow"                        ""]
   ["authorization"                ""]
   ["cache-control"                ""]
   ["content-disposition"          ""]
   ["content-encoding"             ""]
   ["content-language"             ""]
   ["content-length"               ""]
   ["content-location"             ""]
   ["content-range"                ""]
   ["content-type"                 ""]
   ["cookie"                       ""]
   ["date"                         ""]
   ["etag"                         ""]
   ["expect"                       ""]
   ["expires"                      ""]
   ["from"                         ""]
   ["host"                         ""]
   ["if-match"                     ""]
   ["if-modified-since"            ""]
   ["if-none-match"                ""]
   ["if-range"                     ""]
   ["if-unmodified-since"          ""]
   ["last-modified"                ""]
   ["link"                         ""]
   ["location"                     ""]
   ["max-forwards"                 ""]
   ["proxy-authenticate"           ""]
   ["proxy-authorization"          ""]
   ["range"                        ""]
   ["referer"                      ""]
   ["refresh"                      ""]
   ["retry-after"                  ""]
   ["server"                       ""]
   ["set-cookie"                   ""]
   ["strict-transport-security"    ""]
   ["transfer-encoding"            ""]
   ["user-agent"                   ""]
   ["vary"                         ""]
   ["via"                          ""]
   ["www-authenticate"             ""]])

;;; Util

(defn p2n-1
  "'2 to the power of n minus 1'
   '2^n - 1'"
  [n]
  (- (bit-shift-left 1 n) 1))

;;; Encoder

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
  "Encode header"
  [buf]
  (println "encode")
  0)

;;; Decoder

(defprotocol IndexedTableProtocol
  "Access to Header Table"
  (ref-header [this index])
  (add-header! [this header]))

(deftype IndexedTable [^:volatile-mutable table]
  IndexedTableProtocol
  (ref-header [this index] (get table (dec index)))
  (add-header! [this header] (set! table (conj table header))))

(def indexed-table (IndexedTable. static-table))

(def cursor (atom 0))

(defn next-octet
  [buf]
  (get buf (swap! cursor inc)))

(defn current-octet
  [buf]
  (get buf @cursor))

(defn decode-integer-representation
  "6.1 Integer Representation"
  [buf n]
  (let [b (get buf @cursor)
        i (bit-and (p2n-1 n) b)]
    (if (< i (p2n-1 n))
      i
      (loop [b (next-octet buf) i (p2n-1 n) m 0]
        (if (= (bit-and b 128) 0)
          (+ i (* (bit-and b 127) (bit-shift-left 1 m)))
          (recur (next-octet buf) (+ i (* (bit-and b 127) (bit-shift-left 1 m))) (+ m 7)))))))

(defn decode-huffman-string
  "TODO: decode huffman encoded string"
  [string-data]
  "")

(defn decode-raw-string
  "TODO: Decode raw string"
  [buf length]
  (swap! cursor inc)
  (let [data (subvec buf @cursor (swap! cursor #(+ % length)))]
    (if debug? (println "[decode-raw-string] data:" data))
    (apply str (map char data))))

(defn decode-string-literal-representation
  "6.2 String Literal Representation"
  [buf]
  (let [huffman? (bit-test (get buf @cursor) 7)
        string-length (decode-integer-representation buf 7)]
    (if debug? (println "[decode-string-literal-representation] string-length" string-length))
    (if huffman?
      (decode-huffman-string buf string-length)
      (decode-raw-string buf string-length))))

;; 7. Binary Format

(defn indexed-header-field-representation
  "7.1 Indexed Header Field Representation"
  [buf]
  (let [index (decode-integer-representation buf 7)]
    (ref-header indexed-table index)))

(defn literal-header-field-with-incremental-indexing
  "7.2.1 Literal Header Field with Incremental Indexing"
  [buf]
  (defn indexed-name
    "7.2.1 Literal Header Field with Incremental Indexing - Indexed Name"
    [buf]
    (let [index (decode-integer-representation buf 6)
          indexed-header (ref-header indexed-table index)]
      (swap! cursor inc)
      (let [value (decode-string-literal-representation buf)
            header [(first indexed-header) value]]
        (add-header! indexed-table header)
        header)))
  (defn new-name
    "7.2.1 Literal Header Field with Incremental Indexing - New Name"
    [buf]
    (swap! cursor inc)
    (let [name (decode-string-literal-representation buf)
          value (decode-string-literal-representation buf)
          header [name value]]
      (add-header! indexed-table header)
      header))

  (if (= (bit-and (first buf) 2r00111111) 0)
    (new-name buf)
    (indexed-name buf)))

(defn literal-header-field-without-indexing
  "7.2.2 Literal Header Field without Indexing"
  [buf]
  (defn indexed-name
    "7.2.2 Literal Header Field without Indexing - Indexed Name"
    [buf]
    (let [index (decode-integer-representation buf 4)
          indexed-header (ref-header indexed-table index)]
      (swap! cursor inc)
      (let [value (decode-string-literal-representation buf)
            header [(first indexed-header) value]]
      header)))
  (defn new-name
    "7.2.2 Literal Header Field without Indexing - New Name"
    [buf]
    (swap! cursor inc)
    (let [name (decode-string-literal-representation buf)
          value (decode-string-literal-representation buf)
          header [name value]]
      header))

  (if (= (bit-and (first buf) 2r00001111) 0)
    (new-name buf)
    (indexed-name buf)))

(defn literal-header-field-never-indexed
  "7.2.3 Literal Header Field never Indexed"
  [buf]
  (defn indexed-name
    "7.2.3 Literal Header Field never Indexed - Indexed Name"
    [buf]
    (let [index (decode-integer-representation buf 4)
          indexed-header (ref-header indexed-table index)]
      (swap! cursor inc)
      (let [value (decode-string-literal-representation buf)
            header [(first indexed-header) value]]
        header)))
  (defn new-name
    "7.2.2 Literal Header Field never Indexed - New Name"
    [buf]
    (swap! cursor inc)
    (let [name (decode-string-literal-representation buf)
          value (decode-string-literal-representation buf)
          header [name value]]
      header))

  (if debug? (println "literal-header-field-never-indexed"))
  (if (= (bit-and (first buf) 2r00001111) 0)
    (new-name buf)
    (indexed-name buf)))

(defn header-table-size-update
  "7.3 Header Table Size Update"
  [buf]
  0)

(defn decode-header
  [buf]
  (let [head (first buf)]
    (cond
     (bit-test head 7) (indexed-header-field-representation buf)
     (bit-test head 6) (literal-header-field-with-incremental-indexing buf)
     (= (bit-and head 2r11110000) 2r00000000) (literal-header-field-without-indexing buf)
     (= (bit-and head 2r11110000) 2r00010000) (literal-header-field-never-indexed buf)
     (= (bit-and head 2r11110000) 2r00100000) (header-table-size-update buf)
     :else "error")))

(defn decode
  "Decode header data.
   The 'buf' arg should be binary array
   TODO: decode from string"
  [buf]
  (decode-header buf))
