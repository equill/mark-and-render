(in-package :mark-and-render)

;;; In here, we have a parser that transforms regular(ish) wikimarkup into
;;; an AST suitable for feeding to the renderer.
;;; But this one does it character-by-character.

(defun parse-wikimarkup (pagestring)
  "Parent function that takes a string of wikimarkup and returns the parsed AST."
  (with-input-from-string (instr pagestring)
    (markup-to-lists instr nil)))

(defparameter *blocks-including-linebreaks*
  '(:br :p :h1 :h2 :h3 :h4 :h5 :h6 :hr :ul :ol))

(defun markup-to-lists (instr acc)
  "Takes the incoming string of wikimarkup, and returns a list of elements
  to be rendered.
  Arguments:
  - input stream, assumed to be from a string
  - list for accumulation"
  (if (null (peek-char nil instr nil nil))
    ;; If we've hit the end of the string, return the accumulated list
    acc
    ;; If there's still string to be parsed, recursively invoke this function
    ;; on what remains of the string after invoking (start-of-line) and
    ;; concatenating the result of that invocation onto the accumulator.
    (markup-to-lists
      instr
      ;; Insert a <br> after the last element, on the basis that the last
      ;; section was terminated by a Newline or Carriage Return.
      ;; But only if it's something that doesn't already have an equivalent
      ;; effect.
      (if
        (or (null acc)
            (let ((candidate (last acc)))
              (and (consp candidate)
                   (consp (car candidate))
                   (member (caar candidate)
                           *blocks-including-linebreaks*))))
        (nconc acc (start-of-line instr))
        (nconc acc (list (list :br)) (start-of-line instr))))))

(defun cond-append (lst func arg)
  "Helper function to conditionally concatenate a list and the result of
  applying another function to its argument, depending on whether something other
  than nested nulls are returned."
  (let ((result (funcall func arg)))
    ;; If we're not returned a list containing only an empty list...
    (if (and result
             (or (atom result)
                 (car result)))
      (nconc (list lst) result)
      (list lst))))

(defun start-of-line (instr &optional (char-acc "") (list-acc nil))
  "Largely acts as a dispatching function.
  Determines whether the start of the line contains a heading, list item or
  something else of the kind, then passes the result along."
  (let ((c (read-char instr nil nil)))
    (cond
      ;; If we've been handed the end of the string, return the list accumulator
      ((null c)
       list-acc)
      ;; Drop a leading newline,
      ;; under the assumption that it's redundant. If one is relevant,
      ;; it should be handled at the end of a line.
      ;; This does have the implication that you can't pad the beginning of the
      ;; page down with newlines, but that can be worked around by putting a
      ;; space on each line if you really must do that.
      ((or
         (equal c #\Newline)
         (equal c #\Return))
       (start-of-line instr char-acc list-acc))
      ;; Escape the next character
      ((equal c #\\)
       (mid-line instr :escaped t))
      ;; Italic
      ((equal c #\_)
       (cond-append (parse-italic instr) #'mid-line instr))
      ;; Bold
      ((equal c #\*)
       (cond-append (parse-bold instr) #'mid-line instr))
      ;;
      ;; Header lines
      ;;
      ;; Starting with a lower-case 'h'
      ((and
         (equal char-acc "")
         (equal c #\h))
       (start-of-line instr (string c) list-acc))
      ;; if it started with 'h', was that followed by a digit?
      ((and (equal char-acc "h")
            (member c (list #\1 #\2 #\3 #\4 #\5 #\6)))
       (start-of-line instr (concatenate 'string char-acc (string c))))
      ;; if it started with 'h' and a digit, was that followed by a '.'?
      ((and (cl-ppcre:all-matches "h[1-6]" char-acc)
            (equal c #\.))
       (start-of-line instr (concatenate 'string char-acc (string c))))
      ;; if it started with a full header sequence, produce a header line.
      ;; What we need to do here is extract the digit, then assemble a suitable
      ;; :H1-esque keyword from it, and wrap the rest of the line in a list
      ;; starting with that keyword.
      ((and (cl-ppcre:all-matches "h[1-6]\\." char-acc)
            (equal c #\ ))
       (list
         (nconc (list (read-from-string (format nil ":H~A" (subseq char-acc 1 2))))
                (mid-line instr))))
      ;; Anything else
      (t
        (mid-line instr :currstr (string c))))))

(defun mid-line (instream &key (content ()) (currstr nil) (escaped nil))
  "Handles the text within a line, once we've determined its context.
  The content argument is a cons-tree containing what we have so far.
  First argument is a string-stream, such as that provided by (with-input-from-string)"
  (let ((newchar (read-char instream nil)))
    (cond
      ;; Newline
      ((or
         (equal newchar #\Newline)
         (equal newchar #\Return)
         (null newchar))
       (nconc content (list currstr)))
      ;; Start of link markup
      ((and
         (not escaped)
         (equal newchar #\[))
       (mid-line
         instream
         :content (nconc content (list currstr)
                         (list (parse-link instream)))))
      ;; Italic markup
      ((and
         (not escaped)
         (equal newchar #\_))
       (mid-line
         instream
         :content (nconc content (list currstr)
                         (list (parse-italic instream)))))
      ;; Bold markup
      ((and (not escaped)
            (equal newchar #\*))
       (mid-line
         instream
         :content (nconc content (list currstr)
                         (list (parse-bold instream)))))
      ;; Escape the next character
      ((and
         (not escaped)
         (equal newchar #\\))
       (mid-line instream
                 :content content
                 :currstr currstr
                 :escaped t))
      ;; Default: add the current character to the string we're accumulating
      (t
        (mid-line
          instream
          :content content
          :currstr (if currstr
                     (format nil "~a~a" currstr newchar)
                     (string newchar)))))))
