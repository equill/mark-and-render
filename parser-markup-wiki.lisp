(in-package :mark-and-render)

;;; In here, we have a parser that transforms regular(ish) wikimarkup into
;;; an AST suitable for feeding to the renderer.

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

(defun parse-line-remainder (instream &key (content ()) (currstr nil) (escaped nil))
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
       (parse-line-remainder
         instream
         :content (append content (list currstr) (list (parse-italic instream)))))
      ;; Bold markup
      ((and (not escaped)
            (equal newchar #\*))
       (parse-line-remainder
         instream
         :content (append content (list currstr) (list (parse-bold instream)))))
      ;; Escape the next character
      ((and
         (not escaped)
         (equal newchar #\\))
       (parse-line-remainder instream :content content :currstr currstr :escaped t))
      ;; End-of-line
      ((null newchar)
       (append content (list currstr)))
      ;; Add the current character to the end
      ((null currstr)
       (parse-line-remainder
         instream
         :content content
         :currstr (format nil "~a" newchar)))
      (t
        (parse-line-remainder
          instream
          :content content
          :currstr (format nil "~a~a" currstr newchar))))))

(defun parse-line-from-string (line)
  "Parse a string as a line of wikimarkup"
  (if (equal line (format nil "~A" #\Return))
    (list :br)
    (let ((heading (when (> (length line) 3)
                     (get-heading line))))
      (cond
        (heading
          (with-input-from-string (instream (subseq line 4))
            (parse-line-remainder
              instream
              :content (list heading))))
        (t
          (with-input-from-string (instream line)
            (parse-line-remainder
              instream
              :content (list :div))))))))

(defun render-wikimarkup-line (line)
  "Renders a single line of wikimarkup as HTML"
  (with-input-from-string (instr line)
    (with-output-to-string (outstr)
      (princ (emit-html-to-string (parse-line-from-string line)) outstr))))

(defun render-wikimarkup-page (pagestring)
  "Renders a page of wikimarkup as HTML"
  (with-input-from-string (instr pagestring)
    (with-output-to-string (outstr)
      (loop for line = (read-line instr nil)
            while line
            do (princ (emit-html-to-string (parse-line-from-string line))
                      outstr)))))


;;; The object-oriented approach didn't work out quite as well as I'd hoped.
;;; At least, not in the initial form. I may yet revisit this.
#|
#+(or)
(defclass markup-string ()
  ((instring :reader instring
             :initarg :instring
             :initform (error "instring argument is required"))
   (acc :accessor acc
        ;; Accumulator, for storing the intermediate results of parsing a subsection.
        :initform (list))
   (result :accessor result
           ;; This should always be initialised as an empty list
           :initform ())
   (line-start-p :accessor line-start-p
                 ;; Should only be queried by the user, never manipulated
                 :initform t)
   (state :accessor state
          :initform nil)))

#+(or)
(defgeneric process-markup (mstring)
  (:documentation "Parent method for parsing a string of markup."))

#+(or)
(defmethod process-markup ((mstring markup-string))
  (loop for c across (instring mstring)
        do (cond
             ((line-start-p mstring)
              (setf (acc mstring) (list :p (format nil "~A" c)))
              (setf (line-start-p mstring) nil))
             ((eql c #\Newline)
              (push (acc mstring) (result mstring))
              (setf (line-start-p mstring) nil))
             (t
               (setf (cadr (acc mstring))
                     (format nil "~A~A" (cadr (acc mstring)) c))))
        finally (when (acc mstring)
                  (setf (result mstring)
                        (push (acc mstring) (result mstring)))))
  (result mstring))

#+(or)
(defun oo-parse-line (line)
  (process-markup
    (make-instance 'markup-string
                   :instring line)))
|#
