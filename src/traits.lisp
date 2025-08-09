(defpackage game/traits
  (:use :cl)
  (:export #:trait
           #:name
           #:cost
           #:requirement
           #:effect
           #:limits
           #:+dash+
           #:+stealth+
           #:+blade-master+
           #:+sniper+
           #:+riposte+
           #:+push-back+
           #:+shield-bash+
           #:+traits+))

(in-package game/traits)

(defclass trait ()
  ((name        :initarg :name        :initform ""  :accessor name)
   (cost        :initarg :cost        :initform 0   :accessor cost)
   (requirement :initarg :requirement :initform nil :accessor requirement)
   (effect      :initarg :effect      :initform ""  :accessor effect)
   (limits      :initarg :limits      :initform 0   :accessor limits)))

(defmethod print-object ((trait trait) stream)
  (print-unreadable-object (trait stream :type t)
    (if (requirement trait)
        (format stream "~A (~A AP, ~A): ~A (~A)" (name trait) (cost trait) (requirement trait) (effect trait) (limits trait))
        (format stream "~A (~A AP): ~A (~A)" (name trait) (cost trait) (effect trait) (limits trait)))))

(defparameter +dash+ (make-instance 'trait :name "Dash" :effect "Move double speed" :limits 1 :cost 1))
(defparameter +stealth+ (make-instance 'trait :name "Stealth" :effect "DEF+1" :requirement "Move only at half speed" :cost 1))
(defparameter +blade-master+ (make-instance 'trait :name "Blade Master" :effect "Replace primary, or secondary weapon with additional melee weapon" :requirement "Primary, or secondary slot must be empty"))
(defparameter +sniper+ (make-instance 'trait :name "Sniper" :effect "+1 damage at range/-1 damage in melee, can target body parts"))
(defparameter +riposte+ (make-instance 'trait :name "Riposte" :effect "Free counter attack in melee combat if opponent misses"))
(defparameter +push-back+ (make-instance 'trait :name "Push Back" :effect "Pushes enemies out of melee range" :cost 1))
(defparameter +shield-bash+ (make-instance 'trait :name "Shield Bash" :effect "Attack with shield" :cost 1))

(defparameter +traits+ (list +dash+ +stealth+ +blade-master+ +sniper+ +riposte+ +push-back+ +shield-bash+))
