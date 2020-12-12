;;;; clup.asd

(asdf:defsystem #:clup
  :description "Sort password and group files by numeric UIDs."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Modified BSD License"
  :serial t
  :depends-on (:CL-FAD
               :CL-PPCRE
               :RUTILS
               :ALEXANDRIA
               :SPLIT-SEQUENCE
               :COM.DVLSOFT.CLON)
  :pathname "./"
  :components ((:file "package")
               (:file "clup")))
