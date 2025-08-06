(defpackage game/tests/main
  (:use :cl
        :game
        :rove))
(in-package :game/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :game)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))