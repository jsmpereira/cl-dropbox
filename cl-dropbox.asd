;;;; cl-dropbox.asd

(asdf:defsystem #:cl-dropbox
  :serial t
  :description "Common Lisp Client for the Dropbox API."
  :author "José Pereira <jsmpereira@gmail.com>"
  :depends-on (#:drakma
               #:cl-json
               #:cl-oauth)
  :components ((:file "package")
               (:file "cl-dropbox")))

