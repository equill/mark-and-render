(in-package :mark-and-render)

;;; In here, we have a parser that transforms regular(ish) wikimarkup into
;;; an AST suitable for feeding to the renderer.
;;; But this one does it character-by-character.

(defun get-heading (head)
  "If the supplied string starts with .h1 or similar, return an appropriate
  heading keyword."
  (multiple-value-bind
    (confirm result) (cl-ppcre:scan-to-strings "^h([1-6])." head)
    (when confirm
      (read-from-string (format nil ":H~A" (elt result 0))))))

(defun whitespacep (c)
  "Checks whether the supplied character is a whitespace character"
  (member c (list #\Space #\Tab)))

(defun parse-link (instream &optional (link "") (target nil))
  "Parses out a link from the supplied string-stream.
  Arguments:
  - a string-stream, such as that provided by (with-input-from-string)
  - (optional) link title (string)
  - (optional) link target (string)"
  (let ((newchar (read-char instream nil)))
    (cond
      ;; We're at the end of either the link or the whole string
      ((or (null newchar)
           (equal newchar #\Newline)
           (equal newchar #\]))
       (if target
         (list :link :text link target)
         (list :link link)))
      ;; Delimiter between link and target
      ((equal newchar #\|)
       (parse-link instream link ""))
      (target
        (parse-link instream link (format nil "~A~A" target newchar)))
      (t
        (parse-link instream (format nil "~A~A" link newchar) nil)))))

(defun parse-italic (instream &optional (acc ""))
  "Marks up a section of text as being italic
  Arguments:
  - input string-stream, such as that provided by (with-input-from-string)
  - (optional) string of text to be returned as italic section"
  (let ((newchar (read-char instream nil)))
    (cond
      ((or (null newchar)
           (equal newchar #\Newline)
           (equal newchar #\_))
       (list :i acc))
      (t
        (parse-italic instream (format nil "~A~A" acc newchar))))))

(defun parse-bold (instream &optional (acc ""))
  "Marks up a section of text as being italic
  Arguments:
  - input string-stream, such as that provided by (with-input-from-string)
  - (optional) string of text to be returned as italic section"
  (let ((newchar (read-char instream nil)))
    (cond
      ((or (null newchar)
           (equal newchar #\Newline)
           (equal newchar #\*))
       (list :b acc))
      (t
        (parse-bold instream (format nil "~A~A" acc newchar))))))

(defun parse-macro (instream &optional (acc nil) (tag "") (end-tag nil))
  "Takes a section of text wrapped in {macro-name} and {/macro-name} tags,
  and returns a list of the form (:macro-name \"text\")
  It's assumed that a leading '{' has already been encountered and discarded.
  If no matching end-tag is found before the end-of-string is encountered,
  everything between the opening tag and the end of string is still returned
  as a macro section."
  (let ((c (read-char instream nil nil)))
    (cond
      ;; end-of-string after beginning the end-tag
      ((and
         (null c)
         acc
         end-tag)
       (format t "End-of-string before completing the end-tag~%")
       (list (read-from-string (format nil ":~A" tag))
             (format nil "~A{~A" acc end-tag)))
      ;; end-of-string after identifying a tag
      ((and
         (null c)
         acc
         (> (length tag) 0))
       (format t "End-of-string after identifying a tag~%")
       (list (read-from-string (format nil ":~A" tag))
             acc))
      ;; end-of-string and we're still trying to identify a tag
      ((null c)
       (format t "end-of-string and we're still trying to identify a tag~%")
       ;(nconc (list "{") (mid-line tag))
       (format nil "{~A" tag)
       )
      ;; Empty tag - just return NIL
      ((and (equal tag "")
            (equal c #\}))
       (format t "Empty tag; returning NIL~%")
       ())
      ;; We have a full open tag, and now we hit an open-bracket
      ((and (> (length tag) 0)
            acc
            (equal c #\{))
       (format t "We have a full open tag, and now we hit an open-bracket~%")
       (parse-macro instream acc tag ""))
      ;; End-tag lacking the pre-tag forward-slash
      ((and end-tag
            (= (length end-tag) 1)
            (not (equal end-tag "/")))
       (format t "It looked like an end-tag, but there's no leading forward-slash; instead we have '~A'.~%" end-tag)
       (format t "Passing this as the accumulator: '~A'~%" (format nil "~A{~A~A" acc end-tag (string c)))
       ;(parse-macro instream (concatenate 'string acc "{" (string c)) tag)
       (parse-macro instream (format nil "~A{~A~A" acc end-tag (string c)) tag)
       )
      ;; Parsing the end-tag
      ((and end-tag
            (not (equal c #\})))
       (format t "Adding '~A' to the end-tag~%" c)
       (parse-macro instream acc tag (concatenate 'string end-tag (string c))))
      ;; End of the end-tag, and it matches the initial tag
      ((and end-tag
            (equal c #\})
            (equal end-tag (format nil "/~A" tag)))
       (format t "Matched end-tag to tag. Returning...~%")
       (list (read-from-string (format nil ":~A" tag))
             acc))
      ;; End of the end-tag, and it doesn't match the initial tag
      ((and end-tag
            (equal c #\}))
       (format t "End-tag doesn't match the tag~%")
       (parse-macro instream (format nil "~A{~A}" acc end-tag) tag))
      ;; There's at least one character in the tag, and we hit an end-bracket
      ((and (> (length tag) 0)
            (null acc)
            (equal c #\}))
       (format t "Found an end-bracket, after accumulating at least one character in the tag~%")
       (parse-macro instream "" tag))
      ;; Content text
      ((and (> (length tag) 0)
            acc
            (not (equal c #\{)))
       (format t "Accumulating another content-text character: '~A'~%" c)
       (parse-macro instream (concatenate 'string acc (string c)) tag))
      ;; Default case: NFI, right now.
      (t
        (format t "Adding '~A' to the opening tag~%" c)
        (parse-macro instream acc (concatenate 'string tag (string c)))))))

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
  ;; If we've hit the end of the string, return the accumulated list
  (if (null (peek-char nil instr nil nil))
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

(defun start-of-line (instr &optional (char-acc ""))
  "Largely acts as a dispatching function.
  Determines whether the start of the line contains a heading, list item or
  something else of the kind, then passes the result along."
  (let ((c (read-char instr nil nil)))
    (cond
      ;; If we've been handed the end of the string, return the list accumulator
      ((null c)
       nil)
      ;; Drop a leading newline,
      ;; under the assumption that it's redundant. If one is relevant,
      ;; it should be handled at the end of a line.
      ;; This does have the implication that you can't pad the beginning of the
      ;; page down with newlines, but that can be worked around by putting a
      ;; space on each line if you really must do that.
      ((or
         (equal c #\Newline)
         (equal c #\Return))
       (start-of-line instr char-acc))
      ;; Escape the next character
      ((equal c #\\)
       (mid-line instr :escaped t))
      ;; Italic
      ((equal c #\_)
       (cond-append (parse-italic instr) #'mid-line instr))
      ;; Bold
      ((equal c #\*)
       (cond-append (parse-bold instr) #'mid-line instr))
      ;; Macro
      ((equal c #\{)
       (format t "Start-of-line handing over to 'parse-macro...~%")
       (cond-append (parse-macro instr) #'mid-line instr))
      ;;
      ;; Unordered list
      ;;
      ;; Starts with '-'
      ((and (equal char-acc "")
            (equal c #\-))
       (start-of-line instr (string c)))
      ;; Started with a hypen, and a space follows
      ((and (equal char-acc "-")
            (equal c #\Space))
       (unordered-list
         instr
         (list :ul
               (nconc (list :li) (mid-line instr)))))
      ;;
      ;; Header lines
      ;;
      ;; Starting with a lower-case 'h'
      ((and
         (equal char-acc "")
         (equal c #\h))
       (start-of-line instr (string c)))
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
        (mid-line instr :currstr (concatenate 'string char-acc (string c)))))))

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

(defun unordered-list (instream ul-tree &key (char-acc nil))
  "Assembles a <ul><li></li></ul> tree."
  ;; If we've been handed a fresh line that doesn't begin with a hyphen,
  ;; return the UL that we've accumulated thus far.
  ;; Helpfully, this handles end-of-line in the same way by default.
  (if (and (null char-acc) ; (null char-acc) means start-of-line
           (not (member (peek-char nil instream nil nil)
                        (list #\- #\Newline #\Return))))
    (list ul-tree)
    ;; Failing that, let's check what the next character in the stream is
    (let ((c (read-char instream nil nil)))
      (cond
        ;; Newline/Carriage return
        ((or
           (equal c #\Newline)
           (equal c #\Return))
         (unordered-list instream ul-tree))
        ;; If the line starts with "-", it's probably a list-item.
        ;; Send it back for more attention. 
        ((and (null char-acc)
              (equal c #\-))
         (unordered-list instream
                         ul-tree
                         :char-acc (string c)))
        ;; If the line did start with "- ", it's a list-item and should be treated as such.
        ((and (equal char-acc "-")
              (equal c #\Space))
         (unordered-list
           instream
           (nconc ul-tree
                  (list (nconc (list :li) (mid-line instream))))))
        ;; This is a nested list-item.
        ;; Not sure how to handle these right now, so I'm falling back to treating them
        ;; like a list item whose string just happens to start with a hypen.
        ((and (equal char-acc "-")
              (equal c #\-))
         (unordered-list
           instream
           (nconc ul-tree
                  (list (nconc (list :li)
                               (mid-line instream :currstr (string c)))))))
        ;; An odd case in which a second or later line starts with a hypen,
        ;; then a character that's neither a hyphen nor a space.
        ;; For now, I'm just going to fail gracefully and pretend it's a properly-formed
        ;; list-item.
        ;; What I should really do is punt back the accumulated unordered list and then
        ;; process this as a regular line, but I'm too tired to figure out how to do this
        ;; properly right now.
        ((and (equal char-acc "-"))
         (unordered-list
           instream
           (nconc ul-tree
                  (list (nconc (list :li)
                               (mid-line instream :currstr (string c)))))))
        ;; Default case: it's some other kind of line.
        ;; NFI what would fall through here.
        (t
          (error "Somebody sneaked an invalid structure through 'unordered-list: '~A~A'~%"
                 char-acc c))))))
