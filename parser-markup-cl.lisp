(in-package :mark-and-render)

(defun render-multiple-sexprs (instring)
  "Takes an input string containing 1 or more sexprs,
  and renders their contents into a single string.
  Argument: string containing valid markup s-exprs
  Returns: string containing HTML"
  (when (stringp instring)
        (with-output-to-string
         (str1)
         (with-input-from-string
          (input instring)
          (loop for line = (read input nil)
                while line do (emit-html str1 line))))))

(defun string-to-html-string (instring)
  "Takes a string containing a single form of valid markup,
  transforms it to HTML and emits it as a new string.
  Argument: string containing valid markup
  Returns: string containing HTML"
  (emit-html-to-string (read-from-string instring)))

(defun valid-markup-p (markup)
  "Checks whether the input string contains a valid series of markup forms.
  Argument: string
  Returns: boolean"
  (labels ((test-markup (instream)
                        (let ((candidate (read instream nil)))
                          (if (null candidate)
                            t
                            (when (cons-form-p candidate)
                              (test-markup instream))))))
    (with-input-from-string (input markup)
      (test-markup input))))
