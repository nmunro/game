(defsystem "game"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "traits")
                 (:file "status")
                 (:file "weapon")
                 (:file "main"))))
  :description "Generate a skeleton for modern project"
  :in-order-to ((test-op (test-op "game/tests"))))

(defsystem "game/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("game"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for game"
  :perform (test-op (op c) (symbol-call :rove :run c)))
