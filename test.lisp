(in-package :mark-and-render)

;;; Yes, it's about as crude as it gets. But it's still better than nothing.

(defun test ()
  (and
    ;; Wikimarkup
    (equal (mark-and-render::render-content "h1. _foo_ *blah* [linky]")
           "<h1><i>foo</i> <b>blah</b> <a href=\"/pages?title=linky\">linky</a></h1>
")
    ;; s-expr markup
    (equal (mark-and-render::render-content "(:h1 \"title goes here\")")
           "<h1>title goes here</h1>
")))
