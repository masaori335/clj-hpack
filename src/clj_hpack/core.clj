(ns clj-hpack.core)

(def debug? true)

(def static-table
  [["zero"                         nil]
   [":authority"                   ""]
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

(def indexed-table (IndexedTable. '()))

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
  "TODO: Encode header"
  [buf]
  0)

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
        (if (= (bit-and b 128) 0)
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
    (apply str (map char data))))

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
  [buf]
  (let [index (decode-integer-representation buf 7)]
    (ref-header indexed-table index)))

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
  [buf prefix-bit]
  (if debug? (println "[indexed-name]"))
  (let [index (decode-integer-representation buf prefix-bit)
        indexed-header (ref-header indexed-table index)
        value (decode-string-literal-representation buf)
        header [(first indexed-header) value]]
      header))

(defn literal-header-field-with-incremental-indexing
  "7.2.1 Literal Header Field with Incremental Indexing"
  [buf]
  (if debug? (println "[literal-header-field-with-incremental-indexing]"))
  (if-not (= (bit-and (current-octet buf) 2r00111111) 0)
    (let [header (indexed-name buf 6)]
      (add-header! indexed-table header)
      header)
    (do
      (next-octet! buf)
      (let [header (new-name buf)]
        (add-header! indexed-table header)
        header))))

(defn literal-header-field-without-indexing
  "7.2.2 Literal Header Field without Indexing"
  [buf]
  (if debug? (println "[literal-header-field-without-indexing]"))
  (if-not (= (bit-and (current-octet buf) 2r00001111) 0)
    (do
      (indexed-name buf 4))
    (do
      (next-octet! buf)
      (new-name buf))))

(defn literal-header-field-never-indexed
  "7.2.3 Literal Header Field never Indexed"
  [buf]
  (if debug? (println "[literal-header-field-never-indexed]"))
  (if-not (= (bit-and (current-octet buf) 2r00001111) 0)
    (do
      (indexed-name buf 4))
    (do
      (next-octet! buf)
      (new-name buf))))

(defn header-table-size-update
  "7.3 Header Table Size Update"
  [buf]
  0)

(defn decode-header
  [buf]
  (let [head (current-octet buf)]
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
   TODO: decode from string
   TODO: header-table object"
  [buf]
  (if debug? (println "[decode] indexed-table:" indexed-table))
  (if debug? (println "[decode] header-table:" (ref-table indexed-table)))
  (reset! cursor 0)
  (loop [result '[]]
    (if (nil? (current-octet buf))
      (do
        (if debug? (println "[decode] result:" result))
        result)
      (recur (conj result (decode-header buf))))))
