(defsystem psa
  :author "Lewis Weinberger"
  :license "MIT"
  :version "0.1.2"
  :homepage "https://github.com/lewis-weinberger/psa"
  :bug-tracker "https://github.com/lewis-weinberger/psa/issues"
  :source-control (:git "git@github.com:lewis-weinberger/psa.git")
  :description "A simple alerts system using the Discord API."
  :depends-on (:drakma :cl-json :local-time)
  :build-operation "program-op"
  :build-pathname "psa"
  :entry-point "psa:main"
  :components ((:file "psa")))
