(ns clj-hpack.core)

;;
;; Util Functions
;;

(def debug? true)

(defn p2n-1
  "'2 to the power of n minus 1'
   '2^n - 1'"
  [n]
  (- (bit-shift-left 1 n) 1))
