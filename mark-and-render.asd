(defpackage :mar-system
  (:use :common-lisp
        :asdf))
(in-package :mar-system)

(defsystem "mark-and-render"
           :description "Yet another markup library."
           :version "0.1"
           :author "James Fleming"
           :license "LLGPL"
           :depends-on (cl-ppcre)
           :components ((:file "defpackage")
                        (:file "renderer-default")
                        (:file "parser-markup-wiki")
                        (:file "parser-markup-cl")
                        (:file "mar"))
           :serial t)

(defsystem "mark-and-render-tests"
           :description "Test suite for the mark-and-render-package"
           :version "0.1"
           :license "LLGPL"
           :depends-on (mark-and-render
                         eos)
           :components ((:file "defpackage-tests")
                        (:file "mark-and-render-tests"))
           :serial t)
