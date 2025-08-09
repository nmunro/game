(defpackage game/weapon
  (:use :cl)
  (:export #:weapon))

(in-package game/weapon)

(defclass weapon ()
  ((name :initarg :name :initform "" :accessor name)
   (slot :initarg :slot :initform "" :accessor slot)))
