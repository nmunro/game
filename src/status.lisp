(defpackage game/status
  (:use :cl)
  (:export #:status
           #:name
           #:description))

(in-package game/status)

(defclass status ()
  ((name        :initarg :name        :initform (error "Must have a name") :accessor name)
   (description :initarg :description :initform ""                         :accessor description)))

(defparameter +statuses+ (list (make-instance 'status :name "Bleed" :description "Removes 1hp per stack, per turn")))
