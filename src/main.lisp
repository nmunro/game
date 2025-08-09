(defpackage game
  (:use :cl)
  (:export #:main))

(in-package game)

(defun game-loop ()
  nil)

(defun main ()
  (dolist (trait game/traits:+traits+)
    (format t "~A~%" trait)))

(main)
