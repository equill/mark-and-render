(in-package :mark-and-render)

;;; In here, we have a parser that transforms regular(ish) wikimarkup into
;;; an AST suitable for feeding to the renderer.
;;; But this one does it character-by-character.

(defun parse-wikimarkup (pagestring)
  "Parent function that takes a string of wikimarkup and returns the parsed AST."
  (with-input-from-string (instr pagestring)
    (start-of-line instr)))

(defun cond-append (lst func arg)
  "Helper function to conditionally concatenate a list and the result of
  applying another function to its argument, depending on whether something other
  than nested nulls are returned."
  (let ((result (funcall func arg)))
    ;; If we're not returned a list containing only an empty list...
    (if (and result
             (or (atom result)
                 (car result)))
      (append (list lst) result)
      (list lst))))

(defun start-of-line (instr &optional (char-acc "") (list-acc nil))
  "Determines whether the start of the line contains a heading, list item or
  something else of the kind, then passes the result along.
  Currently checking for: :h[1-6]"
  (let ((c (read-char instr nil nil)))
    (cond
      ;; If we've been handed the end of the string, return the list accumulator
      ((null c)
       list-acc)
      ;; Escape the next character
      ((equal c #\\)
       (mid-line instr :escaped t))
      ;; Italic
      ((equal c #\_)
       (cond-append (parse-italic instr) #'mid-line instr))
      ;; Bold
      ((equal c #\*)
       (cond-append (parse-bold instr) #'mid-line instr))
      ;; The beginning of a list
      ((and
         (equal char-acc "")
         (equal c #\:))
       (start-of-line instr (string c) list-acc))
      ;; The beginning of a header line
      ((and (equal char-acc ":")
            (equal c #\h))
       (start-of-line instr (concatenate 'string char-acc (string c)) list-acc))
      ((and (equal char-acc ":h")
            (member c (list #\1 #\2 #\3 #\4 #\5 #\6)))
       (start-of-line instr (concatenate 'string char-acc (string c))))
      ((and (cl-ppcre:all-matches ":h[1-6]" char-acc)
            (equal c #\ ))
       (append (list (read-from-string char-acc))
               (mid-line instr)))
      ;; Anything else
      (t
        (mid-line instr :currstr (string c))))))

(defun mid-line (instream &key (content ()) (currstr nil) (escaped nil))
  "The content argument is a cons-tree containing what we have so far.
  First argument is a string-stream, such as that provided by (with-input-from-string)"
  (let ((newchar (read-char instream nil)))
    (cond
      ;; Start of link markup
      ((and
         (not escaped)
         (equal newchar #\[))
       (parse-line-remainder
         instream
         :content (append content (list currstr) (list (parse-link instream)))))
      ;; Italic markup
      ((and
         (not escaped)
         (equal newchar #\_))
       (mid-line
         instream
         :content (append content (list currstr) (list (parse-italic instream)))))
      ;; Bold markup
      ((and (not escaped)
            (equal newchar #\*))
       (mid-line
         instream
         :content (append content (list currstr) (list (parse-bold instream)))))
      ;; Escape the next character
      ((and
         (not escaped)
         (equal newchar #\\))
       (mid-line instream :content content :currstr currstr :escaped t))
      ;; End-of-line
      ((null newchar)
       (append content (list currstr)))
      (t
        (mid-line
          instream
          :content content
          :currstr (if currstr
                     (format nil "~a~a" currstr newchar)
                     (string newchar)))))))

#+(or)
(defun mid-line (instr &key next-char char-acc (list-acc nil))
  "Parses post-start line content.
  Something of a dispatching function to <em>, <strong> and macros."
  (cond
    ;; We've been passed an empty next-char
    ((equal next-char "")
     ;; If there's a character to read from the string, send it back around
     ;; through this function. Otherwise, return the list accumulator.
     (let ((c (read-char instr nil nil)))
       (if c
         (mid-line instr :next-char (string c) :char-acc "" :list-acc list-acc)
         (list list-acc char-acc))))
    ((equal next-char #\_)
     (mid-line
       instr
       :next-char ""
       :char-acc ""
       :list-acc (append list-acc (list char-acc (parse-italic instr)))))
    (t
      (let ((c (read-char instr nil nil))
            (new-acc (concatenate 'string char-acc (string next-char))))
        (if c
          (mid-line instr :next-char c :char-acc new-acc)
          (append list-acc new-acc))))))
