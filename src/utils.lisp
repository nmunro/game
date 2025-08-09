(defpackage game/utils
  (:use :cl)
  (:export #:main))

(in-package game/utils)

(define-condition invalid-die (error)
  ((msg :initarg :msg :reader msg)))

(defun flip-coin ()
  (if (= (random 2) 0)
      :heads
      :tails))

(defun roll-die (die)
  (handler-case
      (let* ((symbol-string (symbol-name die))
             (pos (position "D" symbol-string :test #'string-equal)))
        (loop :for x :from 1 :to (parse-integer (subseq symbol-string 0 pos))
              :collect (1+ (random (parse-integer (subseq symbol-string (1+ pos)))))))

    (error (error)
      (error 'invalid-die :msg (format nil "~A is not a valid die type" die)))))

(roll-die :2d6)
(roll-die :5d20)
(roll-die "meh") ;; throws an invalid-die error
