;;;
;;; Tests based on examples of HPACK draft-09
;;; Details in http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#appendix-D
;;;


(ns clj-hpack.encode-test
  (:require [clojure.test :refer :all]
            [clj-hpack.encode :refer :all]
            [clj-hpack.header-table :refer :all])
  (:import [clj_hpack.header_table HeaderTable]))

;;; D.1 Integer Representation Examples
;;;   http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#appendix-D.1

(deftest D.1.1
  "D.1.1 Example 1: Encoding 10 Using a 5-bit Prefix"
  (testing ""
    (is (= (encode-integer-representation 10 5) [2r01010]))))

(deftest D.1.2
  "Example 2: Encoding 1337 Using a 5-bit Prefix"
  (testing ""
    (is (= (encode-integer-representation 1337 5) [2r00011111 2r10011010 2r00001010]))))

(deftest D.1.3
  "Example 3: Encoding 42 Starting at an Octet Boundary"
  (testing ""
    (is (= (encode-integer-representation 42 8) [2r00101010]))))

;;; D.2 Header Field Representation Examples
;;;   http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#appendix-D.2

(deftest D.2.1
  "Literal Header Field with Indexing"
  (testing ""
    (let [table (HeaderTable. '())]
      (is (= (encode table [["custom-key" "custom-header"]])
             [0x40 0x0a 0x63 0x75 0x73 0x74 0x6f 0x6d 0x2d 0x6b 0x65 0x79 0x0d 0x63 0x75 0x73 0x74 0x6f 0x6d 0x2d 0x68 0x65 0x61 0x64 0x65 0x72])))))

(deftest D.2.2
  "D.2.2 Literal Header Field without Indexing"
  (let [table (HeaderTable. '())]
    (testing ""
      (is (= (encode table [[":path" "/sample/path"]])
             [0x04 0x0c 0x2f 0x73 0x61 0x6d 0x70 0x6c 0x65 0x2f 0x70 0x61 0x74 0x68])))))

(deftest D.2.3
  "D.2.3 Literal Header Field never Indexed"
  (let [table (HeaderTable. '())]
    (testing ""
      (is (= (encode table [["password" "secret"]])
             [0x10 0x08 0x70 0x61 0x73 0x73 0x77 0x6f 0x72 0x64 0x06 0x73 0x65 0x63 0x72 0x65 0x74])))))

(deftest D.2.4
  "Indexed Header Field"
  (let [table (HeaderTable. '())]
    (testing ""
      (is (= (encode table [[":method" "GET"]])
             [0x82])))))

;;; D.3 Request Examples without Huffman Coding
;;;   http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#appendix-D.3

(deftest D.3.1
  "First Request"
  (testing ""
    (let [table (HeaderTable. '())]
      (is (= (encode table [[":method" "GET"] [":scheme" "http"] [":path" "/"] [":authority" "www.example.com"]])
             [0x82 0x86 0x84 0x41 0x0f 0x77 0x77 0x77 0x2e 0x65 0x78 0x61 0x6d 0x70 0x6c 0x65 0x2e 0x63 0x6f 0x6d])))))

(deftest D.3.2
  "Second Request"
  (testing ""
    (let [table (HeaderTable. '())]
      (is (= (encode table [[":method" "GET"] [":scheme" "http"] [":path" "/"] [":authority" "www.example.com"]])
             [0x82 0x86 0x84 0x41 0x0f 0x77 0x77 0x77 0x2e 0x65 0x78 0x61 0x6d 0x70 0x6c 0x65 0x2e 0x63 0x6f 0x6d]))
      (is (= (encode table [[":method" "GET"] [":scheme" "http"] [":path" "/"] [":authority" "www.example.com"] ["cache-control" "no-cache"]])
             [0x82 0x86 0x84 0xbe 0x58 0x08 0x6e 0x6f 0x2d 0x63 0x61 0x63 0x68 0x65])))))

(deftest D.3.3
  "Third Request"
  (testing ""
    (let [table (HeaderTable. '())]
      (is (= (encode table [[":method" "GET"] [":scheme" "http"] [":path" "/"] [":authority" "www.example.com"]])
             [0x82 0x86 0x84 0x41 0x0f 0x77 0x77 0x77 0x2e 0x65 0x78 0x61 0x6d 0x70 0x6c 0x65 0x2e 0x63 0x6f 0x6d]))
      (is (= (encode table [[":method" "GET"] [":scheme" "http"] [":path" "/"] [":authority" "www.example.com"] ["cache-control" "no-cache"]])
             [0x82 0x86 0x84 0xbe 0x58 0x08 0x6e 0x6f 0x2d 0x63 0x61 0x63 0x68 0x65]))
      (is (= (encode table [[":method" "GET"] [":scheme" "https"] [":path" "/index.html"] [":authority" "www.example.com"] ["custom-key" "custom-value"]])
             [0x82 0x87 0x85 0xbf 0x40 0x0a 0x63 0x75 0x73 0x74 0x6f 0x6d 0x2d 0x6b 0x65 0x79 0x0c 0x63 0x75 0x73 0x74 0x6f 0x6d 0x2d 0x76 0x61 0x6c 0x75 0x65])))))
