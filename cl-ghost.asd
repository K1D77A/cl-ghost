;;;; cl-ghost.asd

(asdf:defsystem #:cl-ghost
  :description "Wrapper for the Content and Admin API for the Ghost CDN https://ghost.org."
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:dexador
               #:jose
               #:ironclad
               #:str
               #:local-time
               #:com.inuoe.jzon)
  :homepage "https://github.com/k1d77a/cl-ghost"
  :bug-tracker "https://github.com/k1d77a/cl-ghost/issues"
  :source-control :GIT
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "classes")
               (:file "conditions")
               (:file "protocol")
               (:file "cl-ghost")
               (:file "content-api")
               (:file "admin-api")))
