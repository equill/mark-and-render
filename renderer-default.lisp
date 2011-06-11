(in-package :mark-and-render)

;;; Shamelessly yanked from chapter 30 of Peter Seibel's marvellous book,
;;; Practical Common Lisp (published by Apress - do buy a copy).
;;; In the first section, I've mostly just changed the layout and added
;;; comments and documentation.
;;;
;;; Except where otherwise noted, copyright belongs to Peter Seibel.
;;; Any bugs, shortcomings and omissions are mine.

(defvar *pretty* t
  "Whether to use the pretty-printer.
  If NIL, the regular print-object is used, which skips all indenting.
  This is typically rebound locally where necessary.")

;; Swiped from Edi Weitz' CL-WHO:
(defparameter *attribute-quote-char* (princ-to-string #\")
  "Quote character for attributes. Defined here so that it can be redefined if you want to use single-quotes, French-style angle-brackets or whatever.")

(defun self-evaluating-p (form)
  "Determines whether an s-expression is a self-evaluating form
  for the purposes of this markup interpreter.
  Argument: s-expr
  Returns: boolean"
  (and (atom form)
       (if (symbolp form)
         (keywordp form)
         t)))

(defun cons-form-p (form &optional (test #'keywordp))
  "Determines whether an s-expression matches either of two forms of markup:
  - ((attributes) content)
  - (:attribute-1 value-1 ...:attribute-n value-n content)
  Arguments:
  - s-expression
  - (optional) test function to determine validity
  Returns:
  - boolean"
  (and (consp form)
       ;; Does it start with a keyword (or other arbitrary item)?
       (or (funcall test (car form))
           ;; ...or does it start with a cons-form?
           (and (consp (car form))
                (funcall test (caar form))))))

(defun parse-cons-form (sexp)
  "Parses a cl-wikimarkup form into components for marking up:
  tag, attribute list, then content. Basically, it homogenises the input
  to make life easy for the actual parser.
  Dispatches to either the explicit-attribute renderer or the
  implicit-attribute one, depending on whether the form starts
  with a cons form. Both return multiple values in the same form.
  Argument:
  - s-expression
  Returns multiple values:
  - tag
  - list of attributes
  - body content"
  (if (consp (first sexp))
    (parse-explicit-attributes-sexp sexp)
    (parse-implicit-attributes-sexp sexp)))

(defun parse-explicit-attributes-sexp (sexp)
  "Breaks down a markup form with explicit attributes into its
  constituent parts: tag, attributes, body content.
  Argument:
  - s-expression starting with a list of attribute-value pairs
  Returns multiple values:
  - tag
  - list of attributes
  - body content"
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  "Breaks down a markup form with implicit attributes into its
  constituent parts: tag, attributes, body content.
  Argument:
  - s-expression starting with a tag, followed by zero or more
    attribute-value pairs, then the content.
  Returns multiple values:
  - tag
  - list of attributes
  - body content"
  (loop with tag = (first sexp)
     for rest on (rest sexp) by #'cddr
     while (and (keywordp (first rest)) (second rest))
     when (second rest)
       collect (first rest) into attributes and
       collect (second rest) into attributes
     end
     finally (return (values tag attributes rest))))

(defun escape-char (char)
  "Replaces dangerous characters with their markup equivalents.
  Argument: character
  Returns: string containing either the canonical form of the character
  or HTML markup"
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape-string (line)
  "Takes a string and renders it safe"
  (with-output-to-string (outstr)
    (loop for c across line
          do (princ (escape-char c) outstr))))

(defun escape (in to-escape)
  "Replaces the specified character in a string with a safely-escaped
  alternative.
  Arguments:
  - string
  - list of characters (those which needs escaping)
  Returns:
  - string (may or may not return the first argument intact)"
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
            for pos = (position-if #'needs-escape-p in :start start)
            do (write-sequence in out :start start :end pos)
            when pos do (write-sequence (escape-char (char in pos)) out)
            while pos))))

(defparameter *element-escapes* "<>&"
  "Characters that need to be escaped in normal HTML element data.")

(defparameter *attribute-escapes* "<>&\"'"
  "Characters that need to be escaped within HTML attribute values.")

(defvar *escapes* *element-escapes*
  "The set of characters that need to be escaped.
  This is prone to being rebound when attributes are being marked up.")

(defclass indenting-printer ()
  ((out :accessor out :initarg :out)
   (beginning-of-line-p :accessor beginning-of-line-p :initform t)
   (indentation :accessor indentation :initform 0)
   (indenting-p :accessor indenting-p :initform t))
  (:documentation "Wraps around an output stream.
  Instances are used by functions to emit strings to the stream while
  keeping track of when they're at the beginning of the line.
  'indentation defines the number of leading spaces.
  'indenting-p determines whether the line should be indented."))

(defun emit (ip string)
  "Emits a string to the output stream of an indenting-printer object,
  resetting the object's beginning-of-line-p slot whenever it emits a
  newline. Called for its side-effect rather than its return value.
  Arguments:
  - indenting-printer object
  - string
  Returns:
  - nothing of interest"
  (loop for start = 0 then (1+ pos)
     for pos = (position #\Newline string :start start)
     do (emit/no-newlines ip string :start start :end pos)
     when pos do (emit-newline ip)
     while pos))

(defun emit/no-newlines (ip string &key (start 0) end)
  "Writes a string to the output stream of an indenting-printer object,
  emitting any required indentation beforehand.
  Arguments:
  - indenting-printer object
  - string to be printed and conditionally indented
  - (key) start: starting position within the string
  - (key) end: whether to start printing from the far end
  The keyword arguments are passed to (write-sequence-string).
  Returns:
  - nothing of interest"
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string))
                    start))
    (setf (beginning-of-line-p ip) nil)))

(defun indent-if-necessary (ip)
  "Checks 'beginning-of-line-p and 'indenting-p to see whether indentation is
  required, and emits the number of spaces defined in 'indentation.
  Argument: indenting-printer object
  Returns: nothing of interest"
  (when (and (beginning-of-line-p ip) (indenting-p ip))
    (loop repeat (indentation ip) do (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))

(defun emit-newline (ip)
  "Emits a newline to the output stream.
  Argument: indenting-printer object
  Returns: nothing of interest"
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  "Ensures the current line begins after a newline.
  Attempts to avoid inserting additional newlines.
  Argument: indenting-printer object
  Returns: nothing of interest"
  (unless (beginning-of-line-p ip) (emit-newline ip)))

(defgeneric
  raw-string (processor string &optional newlines-p)
  (:documentation
    "For emitting strings that don't need character-escaping.
    newlines-p is there in case you need to pass it a string containing
    one or more newlines."))

(defgeneric newline (processor))

(defgeneric freshline (processor))

(defgeneric indent (processor))

(defgeneric unindent (processor))

(defgeneric toggle-indenting (processor))

(defclass html-pretty-printer ()
  ((printer   :accessor printer   :initarg :printer)
   (tab-width :accessor tab-width :initarg :tab-width :initform 2))
  (:documentation
    "Object for co-ordinating the pretty-printing implementations of the
    output-related printing methods.
    'printer should contain an indenting-printer object.
  'tab-width defines the number of spaces by which each nested level of
  indentation should be increased."))

(defmethod raw-string ((pp html-pretty-printer) string &optional newlines-p)
  (if newlines-p
    (emit (printer pp) string)
    (emit/no-newlines (printer pp) string)))

(defmethod newline ((pp html-pretty-printer))
  (emit-newline (printer pp)))

(defmethod freshline ((pp html-pretty-printer))
  (when *pretty* (emit-freshline (printer pp))))

(defmethod indent ((pp html-pretty-printer))
  (when *pretty*
    (incf (indentation (printer pp)) (tab-width pp))))

(defmethod unindent ((pp html-pretty-printer))
  (when *pretty*
    (decf (indentation (printer pp)) (tab-width pp))))

(defmethod toggle-indenting ((pp html-pretty-printer))
  (when *pretty*
    (with-slots (indenting-p) (printer pp)
      (setf indenting-p (not indenting-p)))))

(defun process (processor form)
  "Converts markup to HTML.
  Arguments:
  - processor object, e.g. html-pretty-printer instance
  - s-expression in the form of valid markup
  Returns:
  - string containing HTML"
  (cond
    ((null form)
     "")
    ((sexp-html-p form)
     (process-sexp-html processor form))
    (t
      (error "Malformed FOO form: ~s" form))))

(defun sexp-html-p (form)
  "Determines whether an object is a valid chunk of markup.
  Argument: s-expression
  Returns: boolean"
  (or (self-evaluating-p form)
      (cons-form-p form)))

(defun process-sexp-html (processor form)
  "Dispatches markup for processing according to whether it's
  self-evaluating.
  Arguments:
  - processor, e.g. html-pretty-printer
  - s-expression (valid markup, please)
  Returns:
  - string containing HTML"
  (if (self-evaluating-p form)
    (raw-string processor (escape (princ-to-string form) *escapes*) t)
    (process-cons-sexp-html processor form)))

(defparameter *block-elements*
  '(:body :colgroup :dl :fieldset :form :head :html :map :noscript :object
    :ol :optgroup :pre :script :select :style :table :tbody :tfoot :thead
    :tr :ul)
  "Elements that need indenting as they're nested, as well as fresh lines.")

(defparameter *paragraph-elements*
  '(:area :base :blockquote :br :button :caption :col :dd :div :dt :h1
    :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
    :td :textarea :th :title)
  "Elements that need fresh lines before being emitted.")

(defparameter *inline-elements*
  '(:a :abbr :acronym :address :b :bdo :big :cite :code :del :dfn :em
       :i :img :ins :kbd :label :legend :q :samp :small :span :strong :sub
       :sup :tt :var)
  "Elements to be emitted inline; that is, they need neither freshlines
nor indentation.")

(defun block-element-p (tag)
  "Determines whether an element is a block-level element, and thus
  in need of a freshline and indentation."
  (find tag *block-elements*))

(defun paragraph-element-p (tag)
  "Determines whether an element is a paragraph-level element, and thus
  in need of a freshline."
  (find tag *paragraph-elements*))

(defparameter *empty-elements*
  '(:area :base :br :col :hr :img :input :link :meta :param)
  "Self-closing elements that don't require a separate closing tag.")

(defun empty-element-p (tag)
  "Determines whether an element is an empty one, not requiring a
  closing tag."
  (find tag *empty-elements*))

(defparameter *preserve-whitespace-elements*
  '(:pre :script :style)
  "Elements whose whitespace must be preserved on the browser screen.")

(defun preserve-whitespace-p (tag)
  "Determines whether the tag identifies content whose whitespace must be
  preserved."
  (find tag *preserve-whitespace-elements*))

(defparameter *xhtml* nil
  "States whether we're emitting XHTML.
  This affects the treatment of empty elements.")

(defun emit-open-tag (processor tag body-p attributes)
  "Emits the opening tag of an element to the output stream.
  Arguments:
  - processor, e.g. html-pretty-printer instance
  - keyword symbol (the tag)
  - boolean indicating the presence or absence of an actual body
  - a list of zero or more attributes
  Returns:
  - nothing of interest"
  (when (or (paragraph-element-p tag)
            (block-element-p tag))
    (freshline processor))
  (raw-string processor (format nil "<~(~a~)" tag))
  (emit-attributes processor attributes)
  (raw-string processor (if (and *xhtml* (not body-p))
                          "/>"
                          ">")))

;; Modified to use the *attribute-quote-char* variable,
;; as swiped from CL-WHO.
;; The idea is to allow you to tailor the quoting to suit your
;; purposes, as neither ' nor " is a good choice in _all_ cases.
(defun emit-attributes (processor attributes)
  "Emits the attributes of an HTML element to the output stream.
  Wants hacking in the event of browsers that want boolean attributes
  specified by themselves instead of as 'attributename' = 'attributename'.
  Arguments:
  - processor object, e.g. html-pretty-printer
  - list of zero or more attributes
  Returns:
  - nothing of interest"
  (loop for (k v) on attributes by #'cddr do
        (raw-string processor (format nil " ~(~a~)=~a"
                                      k *attribute-quote-char*))
        (let ((*escapes* *attribute-escapes*))
          (process processor (if (eql v t) (string-downcase k) v)))
        (raw-string processor *attribute-quote-char*)))

(defun emit-element-body (processor tag body)
  "Emits the body of an HTML element to the output stream.
  Arguments:
  - processor object, e.g. html-pretty-printer instance
  - keyword symbol, being the tag within which it's contained
  - body content of the element
  Returns:
  - nothing of interest"
  (when (block-element-p tag)
    (freshline processor)
    (indent processor))
  (when (preserve-whitespace-p tag) (toggle-indenting processor))
  (dolist (item body)  (process processor item))
  (when (preserve-whitespace-p tag) (toggle-indenting processor))
  (when (block-element-p tag)
    (unindent processor)
    (freshline processor)))

(defun emit-close-tag (processor tag body-p)
  "Emits the closing tag of an HTML element.
  Arguments:
  - processor object, e.g. html-pretty-printer instance
  - keyword symbol, being the tag in question
  - boolean indicating the presence or absence of an actual body
  Returns:
  - nothing of interest"
  (unless (and (or *xhtml* (empty-element-p tag)) (not body-p))
    (raw-string processor (format nil "</~(~a~)>" tag)))
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor)))

(defun emit-html (outstr sexp)
  "Transforms a markup form to HTML and emits it to an output stream.
  Argument:
  - output stream
  - s-expression in the form of valid markup
  Returns: nothing of interest"
  (process (get-pretty-printer outstr) sexp))

(defun get-pretty-printer (outstr)
  "Generates a fresh pretty-printer object."
  (make-instance
   'html-pretty-printer
   :printer (make-instance 'indenting-printer
                           :out outstr)))

;;; My additions and alterations to the library


(defvar *macro-hash* (make-hash-table :test #'eql)
  "Hash-table containing the functions to be called in response to
  'macro' wikimarkup taqs.
  The keys are keyword symbols.
  The values are functions whose arguments are the attributes and body
  returned by (parse-cons-form).
  The functions are expected to return standard wikimarkup tag,
  attributes and body via (values) in the same manner as
  (parse-cons-form).")

;; Originally Peter Seibel's.
;; Hacked up by me to include a hook for macros
(defun process-cons-sexp-html (processor form)
  "Transform a section of markup into HTML, and emits it to
  the output stream.
  Arguments:
  - processor object, e.g. html-pretty-printer instance
  - s-expression in the form of valid markup
  Returns:
  - nothing of interest"
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  ;; The code's ugly, but I haven't yet spotted a more elegant way of
  ;; conditionally-rebinding the values that wouldn't be even worse.
  ;
  ;; Parse the incoming form into the tag, the attributes and the body
  (multiple-value-bind
   (tag attributes body) (parse-cons-form form)
   ;; Look for a wiki-macro corresponding to this tag
   (let ((m-function (gethash tag *macro-hash*)))
        ;; If we find a wiki-macro corresponding to the tag,
        ;; feed it the attributes and body, and process the result.
        (if m-function
            (multiple-value-bind
             (new-tag new-attributes new-body) (funcall m-function attributes body)
             (emit-open-tag     processor new-tag new-body new-attributes)
             (emit-element-body processor new-tag new-body)
             (emit-close-tag    processor new-tag new-body))
            ;; If there wasn't a wiki-macro, process in the normal manner
            (progn
             (emit-open-tag     processor tag body attributes)
             (emit-element-body processor tag body)
             (emit-close-tag    processor tag body))))))


;; Custom wikimarkup macros

(setf (gethash :link *macro-hash*)
      (lambda (attributes target)
        ":link macro, for automagically linking to other pages by title.
        I expect to expand this to handle links to other kinds of content,
        such as attachments, but for now we only do pages."
        (let* ((link-target (car target))
               (text (getf attributes :text))
               (display-text (if text text link-target)))
          (values :a
                  (list :href link-target)
                  (list display-text)))))


;; Handy hook for invoking all the above

(defun emit-html-to-string (sexp &optional outstr)
  "Transforms a single s-expr to HTML, and returns the result as a string.
  Argument: s-expr in valid markup
  Returns: string"
  (if outstr
    (emit-html outstr sexp)
    (with-output-to-string (str1) (emit-html str1 sexp))))
