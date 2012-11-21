(defsystem "mark-and-render-tests"
           :description "Test suite for the mark-and-render-package"
           :version "0.1"
           :license "LLGPL"
           :depends-on (mark-and-render
                         fiveam)
           :components ((:file "defpackage-tests")
                        (:file "mark-and-render-tests"))
           :serial t)
