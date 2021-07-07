(asdf:defsystem shedulr
  :name "shedulr"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description ""
  :licence "MIT"
  :build-operation "asdf:program-op"
  :build-pathname "shedulr"
  :entry-point "shedulr:repl"
  :depends-on ("cl-bcrypt" "local-time" "uuid" "whirlog")
  :serial t
  :components ((:file "hash")
	       (:file "time")
	       (:file "unique")
	       (:file "shedulr")))
