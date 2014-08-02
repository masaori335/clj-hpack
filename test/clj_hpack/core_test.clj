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
    (is (= (encode 10) 2r01010))))

(deftest D.1.2
  "Example 2: Encoding 1337 Using a 5-bit Prefix"
  (testing ""
    (is (= (encode 1337) 2r00001010))))

(deftest D.1.3
  "Example 3: Encoding 42 Starting at an Octet Boundary"
  (testing ""
    (is (= (encode 42) 2r00101010))))

;;; D.2 Header Field Representation Examples

(deftest D.2.1
  "Literal Header Field with Indexing"
  (testing ""
    (is (= (decode [0x400a 0x6375 0x7374 0x6f6d 0x2d6b 0x6579 0x0d63 0x7573 0x746f 0x6d2d 0x6865 0x6164 0x6572]) "custom-key: custom-header"))))

(deftest D.2.4
  "Indexed Header Field"
  (testing ""
    (is (= (decode 82) ":method: GET"))))

;;; D.3 Request Examples without Huffman Coding

(deftest D.3.1
  "First Request"
  (testing ""
    (is (= (decode [0x8286 0x8441 0x0f77 0x7777 0x2e65 0x7861 0x6d70 0x6c65 0x2e63 0x6f6d])
           [":method: GET" ":scheme: http" ":path: /" ":authority: www.example.com"]))))

(deftest D.3.2
  "Second Request"
  (testing ""
    (is (= (decode [0x8286 0x84be 0x5808 0x6e6f 0x2d63 0x6163 0x6865])
           [":method: GET" ":scheme: http" ":path: /" ":authority: www.example.com" "cache-control: no-cache"]))))

(deftest D.3.3
  "Third Request"
  (testing ""
    (is (= (decode [0x8287 0x85bf 0x400a 0x6375 0x7374 0x6f6d 0x2d6b 0x6579 0x0c63 0x7573 0x746f 0x6d2d 0x7661 0x6c75 0x65])
           [":method: GET" ":scheme: https" ":path: /index.html" ":authority: www.example.com" "custom-key: custom-value"]))))
