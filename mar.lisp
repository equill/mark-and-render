(in-package :mark-and-render)

(defun render-content (instr &key (pretty *pretty*))
  "Transform the content of a page object into HTML.
  We test all forms within the string, and fall through to plain text
  unless all of them are valid. Could be a bit rough when the typo's at
  the end of the page, but I'm sure we can get more sophisticated later.
  Argument: wiki-page object
  Returns: a string containing the rendered output"
  (let ((*pretty* pretty))
    (cond
      ;; A string containing valid markup
      ((and
         (stringp instr)
         (valid-markup-p instr))
       (render-multiple-sexprs instr))
      ;; A plain string
      ((stringp instr)
       (render-wikimarkup-page instr))
      ;; Valid markup
      ((consp instr)
       (emit-html-to-string instr))
      ;; We don't know what to do with anything else, so return an empty string.
      (t
        ""))))
