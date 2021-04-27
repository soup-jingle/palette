(defpackage palette/tests/main
  (:use :cl
        :palette
        :rove))
(in-package :palette/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :palette)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
