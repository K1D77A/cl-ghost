;;;; cl-ghost.asd

(asdf:defsystem #:cl-ghost
  :description "Describe cl-ghost here"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:dexador
               #:jose
               #:ironclad
               #:str
               #:local-time)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "cl-ghost")))
