;;;
;;; Tests based on examples of HPACK draft-09
;;; Details in http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#appendix-D
;;;


(ns clj-hpack.core-test
  (:require [clojure.test :refer :all]
            [clj-hpack.core :refer :all]))

;;; D.1 Integer Representation Examples

(deftest D.1.1
  "D.1.1 Example 1: Encoding 10 Using a 5-bit Prefix"
  (testing ""
    (is (= (encode-integer-representation 10 5) [2r01010]))))

(deftest D.1.1-decode
  "D.1.1 Example 1: Encoding 10 Using a 5-bit Prefix"
  (reset! cursor 0)
  (testing ""
    (is (= (decode-integer-representation [2r01010] 5) 10))))

(deftest D.1.2
  "Example 2: Encoding 1337 Using a 5-bit Prefix"
  (testing ""
    (is (= (encode-integer-representation 1337 5) [2r00011111 2r10011010 2r00001010]))))

(deftest D.1.2-decode
  "Example 2: Encoding 1337 Using a 5-bit Prefix"
  (reset! cursor 0)
  (testing ""
    (is (= (decode-integer-representation [2r00011111 2r10011010 2r00001010] 5) 1337))))

(deftest D.1.3
  "Example 3: Encoding 42 Starting at an Octet Boundary"
  (testing ""
    (is (= (encode-integer-representation 42 8) [2r00101010]))))

(deftest D.1.3-decode
  "Example 3: Encoding 42 Starting at an Octet Boundary"
  (reset! cursor 0)
  (testing ""
    (is (= (decode-integer-representation [2r00101010] 8) 42))))

;;; D.2 Header Field Representation Examples

(deftest D.2.1
  "Literal Header Field with Indexing"
  (reset! cursor 0)
  (testing ""
    (is (= (decode [0x40 0x0a 0x63 0x75 0x73 0x74 0x6f 0x6d 0x2d 0x6b 0x65 0x79 0x0d 0x63 0x75 0x73 0x74 0x6f 0x6d 0x2d 0x68 0x65 0x61 0x64 0x65 0x72])
           [["custom-key" "custom-header"]]))))

(deftest D.2.2
  "D.2.2 Literal Header Field without Indexing"
  (reset! cursor 0)
  (testing ""
    (is (= (decode [0x04 0x0c 0x2f 0x73 0x61 0x6d 0x70 0x6c 0x65 0x2f 0x70 0x61 0x74 0x68])
           [[":path" "/sample/path"]]))))

(deftest D.2.3
  "D.2.3 Literal Header Field never Indexed"
  (reset! cursor 0)
  (testing ""
    (is (= (decode [0x10 0x08 0x70 0x61 0x73 0x73 0x77 0x6f 0x72 0x64 0x06 0x73 0x65 0x63 0x72 0x65 0x74])
           [["password" "secret"]]))))

(deftest D.2.4
  "Indexed Header Field"
  (reset! cursor 0)
  (testing ""
    (is (= (decode [0x82])
           [[":method" "GET"]]))))

;;; D.3 Request Examples without Huffman Coding

;; (deftest D.3.1
;;   "First Request"
;;   (testing ""
;;     (is (= (decode [0x82 0x86 0x84 0x41 0x0f 0x77 0x77 0x77 0x2e 0x65 0x78 0x61 0x6d 0x70 0x6c 0x65 0x2e 0x63 0x6f 0x6d])
;;            [":method: GET" ":scheme: http" ":path: /" ":authority: www.example.com"]))))

;; (deftest D.3.2
;;   "Second Request"
;;   (testing ""
;;     (is (= (decode [0x8286 0x84be 0x5808 0x6e6f 0x2d63 0x6163 0x6865])
;;            [":method: GET" ":scheme: http" ":path: /" ":authority: www.example.com" "cache-control: no-cache"]))))

;; (deftest D.3.3
;;   "Third Request"
;;   (testing ""
;;     (is (= (decode [0x8287 0x85bf 0x400a 0x6375 0x7374 0x6f6d 0x2d6b 0x6579 0x0c63 0x7573 0x746f 0x6d2d 0x7661 0x6c75 0x65])
;;            [":method: GET" ":scheme: https" ":path: /index.html" ":authority: www.example.com" "custom-key: custom-value"]))))
