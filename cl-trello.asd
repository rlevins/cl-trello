;;;; cl-trello.asd

#-asdf3.1 (error "cl-sfdc requires ASDF 3.1")

(asdf:defsystem #:cl-trello
  :description "Describe cl-trello here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version (:read-file-form "cl-trello.lisp" :at (1 2))
  :depends-on (#:drakma
	       #:cl-json
	       #:local-time
	       #:log4cl)
  :serial t
  :components ((:file "package")
               (:file "cl-trello")))

