(defpackage game
  (:use :cl)
  (:export #:main))

(in-package game)

(defclass trait ()
  ((name        :initarg :name        :initform "" :accessor name)
   (cost        :initarg :cost        :initform 0  :accessor cost)
   (requirement :initarg :requirement :initform "" :accessor requirement)
   (effect      :initarg :effect      :initform "" :accessor effect)
   (limits      :initarg :limits      :initform 0  :accessor limits)))

(defmethod print-object ((trait trait) stream)
  (print-unreadable-object (trait stream :type t)
    (format stream "Name: ~A" (name trait))))

(defparameter +traits+ `(
  ,(make-instance 'trait :name "Dash" :effect "Move double speed" :limits 1 :cost 1)
  ,(make-instance 'trait :name "Stealth" :effect "DEF+1" :requirement "1 full turn in cover, move only at half speed" :cost 1)
  ,(make-instance 'trait :name "Blade Master" :effect "Replace primary, or secondary weapon with additional melee weapon" :requirement "Primary, or secondary slot must be empty")
  ,(make-instance 'trait :name "Sniper" :effect "+1 damage at range/-1 damage in melee, can target body parts")
  ,(make-instance 'trait :name "Riposte" :effect "Free counter attack in melee combat if opponent misses")
  ,(make-instance 'trait :name "Push Back" :effect "Pushes enemies out of melee range" :cost 1)
  ,(make-instance 'trait :name "Shield Bash" :effect "Attack with shield" :cost 1)))

(defun flip-coin ()
  (if (= (random 2) 0)
      :heads
      :tails))

(defun roll-die (die)
  (1+ (random die)))

(defun roll-dice (num type)
  (loop :for x :from 1 :to num :collect (roll-die type)))

(defun main ()
  (dolist (trait +traits+)
    (format t "Trait: ~A~%" trait)))

(main)
