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
  :depends-on ("local-time" "pardom" "uuid" "whirlog" "woo")
  :serial t
  :components ((:file "id")
	       (:file "ls")
	       (:file "shedulr")))
