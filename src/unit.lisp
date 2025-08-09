(defpackage game/unit
  (:use :cl)
  (:export #:main))

(in-package game/unit)

(defclass body-part ()
  ((hit-points :initarg :hp :initform (error "Must provide a hp value") :accessor hp)))

(defmethod print-object ((body-part body-part) stream)
  (print-unreadable-object (body-part stream)
    (format stream "~A: ~A/~A" (name body-part) (- (hp body-part) (damage body-part)) (hp body-part))))

(defclass head (body-part)
  ((hit-points :initarg :hp :initform 3        :accessor hp)
   (name       :initarg :name :initform "head" :accessor name)
   (damage     :initarg :damage :initform 0    :accessor damage)))

(defclass torso (body-part)
  ((hit-points :initarg :hp     :initform 6       :accessor hp)
   (name       :initarg :name   :initform "torso" :accessor name)
   (damage     :initarg :damage :initform 0       :accessor damage)))

(defclass right-leg (body-part)
  ((hit-points :initarg :hp     :initform 4           :accessor hp)
   (name       :initarg :name   :initform "right leg" :accessor name)
   (damage     :initarg :damage :initform 0           :accessor damage)))

(defclass left-leg (body-part)
  ((hit-points :initarg :hp     :initform 4           :accessor hp)
   (name       :initarg :name   :initform "left leg"  :accessor name)
   (damage     :initarg :damage :initform 0           :accessor damage)))

(defclass right-arm (body-part)
  ((hit-points :initarg :hp     :initform 4           :accessor hp)
   (name       :initarg :name   :initform "right arm" :accessor name)
   (damage     :initarg :damage :initform 0           :accessor damage)))

(defclass left-arm (body-part)
  ((hit-points :initarg :hp     :initform 4           :accessor hp)
   (name       :initarg :name   :initform "left arm"  :accessor name)
   (damage     :initarg :damage :initform 0           :accessor damage)))

(defclass squad-character ()
  ((name          :initarg :name     :initform (error "Must have a name") :accessor name)
   (action-points :initarg :ap       :initform 2                          :accessor ap)
   (hit-points    :initarg :hp       :initform 25                         :accessor hp)
   (head          :initarg :head     :initform (make-instance 'head)      :accessor head)
   (torso         :initarg :torso    :initform (make-instance 'torso)     :accessor torso)
   (right-arm     :initarg :torso    :initform (make-instance 'right-arm) :accessor right-arm)
   (left-arm      :initarg :torso    :initform (make-instance 'left-arm)  :accessor left-arm)
   (right-leg     :initarg :torso    :initform (make-instance 'right-leg) :accessor right-leg)
   (left-leg      :initarg :torso    :initform (make-instance 'left-leg)  :accessor left-leg)
   (defense       :initarg :def      :initform 4                          :accessor def)
   (speed         :initarg :spd      :initform 10                         :accessor spd)
   ;; 1 - 5, roll 1d6, crit of 1 means a 5 is needed, 2 means a 4 is needed etc
   (critical      :initarg :crt      :initform 1                          :accessor crt)
   (traits        :initarg :traits   :initform nil                        :accessor traits)
   (load-out      :initarg :load-out :initform nil                        :accessor load-out)
   (status        :initarg :status   :initform "Healthy"                  :accessor status)))

;; @TODO: Status effect needs to be added to character, then the effects * stacks need to be applied to the character
(defgeneric inflict-status-stack (squad-character status)
  (:documentation "Inflicts a status stack on a character"))

(defmethod inflict-status-stack (squad-character status)
  nil)

(defgeneric apply-status-effects (squad-character)
  (:documentation "Applies the status effect to a character multiplied by the stacks"))

(defmethod apply-status-effects ((squad-character squad-character))
  nil)

(defmethod print-object ((squad-character squad-character) stream)
  (print-unreadable-object (squad-character stream)
    (format stream "~A (~A):~%AP: ~A | HP: ~A | DEF: ~A | SPD: ~A | CRT: ~A~%~A~%~A~%~A~%~A~%~A~%~A"
            (name squad-character)
            (status squad-character)
            (ap squad-character)
            (hp squad-character)
            (def squad-character)
            (spd squad-character)
            (crt squad-character)
            (head squad-character)
            (torso squad-character)
            (right-arm squad-character)
            (left-arm squad-character)
            (right-leg squad-character)
            (left-leg squad-character))))
