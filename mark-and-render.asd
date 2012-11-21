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
