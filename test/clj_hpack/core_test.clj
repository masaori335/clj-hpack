;;;
;;; Tests based on examples of HPACK draft-09
;;; Details in http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#appendix-D
;;;


(ns clj-hpack.core-test
  (:require [clojure.test :refer :all]
            [clj-hpack.core :refer :all]))

;;; D.1 Integer Representation Examples
;;;   http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09#appendix-D.1

(deftest test_of_p2n-1
  "Power of 2 minus 1"
  (testing "n = 0" (is (= (p2n-1 0) 2r0)))
  (testing "n = 1" (is (= (p2n-1 1) 2r1)))
  (testing "n = 2" (is (= (p2n-1 2) 2r11)))
  (testing "n = 3" (is (= (p2n-1 3) 2r111)))
  (testing "n = 4" (is (= (p2n-1 4) 2r1111))))
