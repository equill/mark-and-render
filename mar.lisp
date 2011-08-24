(in-package :mark-and-render)

(defun auto-render-content (instr &key (pretty *pretty*))
  "Demo function; its use is deprecated in favour of embedding suitable logic
  into the calling application.
  Transforms the content of a page object into HTML, auto-detecting the type
  of input.
  We test all forms within the string, and fall through to plain text
  if any of them are invalid. Makes for something of a performance cost when the
  typo's at the end of a long page, but I'm sure we can get more sophisticated
  if necessary.
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
