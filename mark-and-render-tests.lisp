(in-package :mark-and-render-tests)

;;; Short instructions:
;;; (asdf:load-system :mark-and-render-tests)
;;; (in-package :mark-and-render-tests)
;;; (eos:run! 'mark-and-render-tests:m-a-r)


(fiveam:def-suite m-a-r)
(fiveam:in-suite m-a-r)

(fiveam:test
  (parse-checks)
  ;; Regular, plain text
  (fiveam:is (equal '("foo") (parse-wikimarkup "foo")))
  (fiveam:is (equal '("foo bar") (parse-wikimarkup "foo bar")))
  ;;
  ;; Italic at start of line
  (fiveam:is (equal '((:i "foo")) (parse-wikimarkup "_foo_")))
  ;;
  ;; Escaped italic at start of line
  (fiveam:is (equal '("_foo_") (parse-wikimarkup "\\_foo\\_")))
  ;;
  ;; Bold at start of line
  (fiveam:is (equal '((:b "foo")) (parse-wikimarkup "*foo*")))
  ;;
  ;; Escaped bold at start of line
  (fiveam:is (equal '("*foo*") (parse-wikimarkup "\\*foo\\*")))
  ;;
  ;; Leading italic followed by regular text
  (fiveam:is (equal '((:i "foo") " bar") (parse-wikimarkup "_foo_ bar")))
  ;;
  ;; Leading italic followed by regular text
  (fiveam:is (equal '((:b "foo") " bar") (parse-wikimarkup "*foo* bar")))
  ;;
  ;; Escaped italic in mid-line
  (fiveam:is (equal '("bar _foo_") (parse-wikimarkup "bar \\_foo\\_")))
  ;;
  ;; Escaped bold in mid-line
  (fiveam:is (equal '("bar *foo*") (parse-wikimarkup "bar \\*foo\\*")))
  ;;
  ;; FIXME
  ;; The next two shouldn't have the trailing 'nil, but I've
  ;; yet to figure out how to eliminate it.
  ;; Leading bold with mid-line italic
  (fiveam:is (equal '((:b "foo") " " (:i "bar") nil) (parse-wikimarkup "*foo* _bar_")))
  ;;
  ;; Leading italic with mid-line bold
  (fiveam:is (equal '((:i "foo") " " (:b "bar") nil) (parse-wikimarkup "_foo_ *bar*")))
  ;;
  ;; Leading bold with mid-line italic and trailing normal text
  (fiveam:is (equal '((:b "foo") " " (:i "bar") " baz") (parse-wikimarkup "*foo* _bar_ baz")))
  ;;
  ;; Leading bold with mid-line italic and unspaced trailing normal text
  (fiveam:is (equal '((:b "foo") " " (:i "bar") "baz") (parse-wikimarkup "*foo* _bar_baz")))
  ;;
  ;; Headings
  (fiveam:is (equal '((:h1 "foo")) (parse-wikimarkup "h1. foo")))
  (fiveam:is (equal '((:h2 "foo")) (parse-wikimarkup "h2. foo")))
  (fiveam:is (equal '((:h3 "foo")) (parse-wikimarkup "h3. foo")))
  (fiveam:is (equal '((:h4 "foo")) (parse-wikimarkup "h4. foo")))
  (fiveam:is (equal '((:h5 "foo")) (parse-wikimarkup "h5. foo")))
  (fiveam:is (equal '((:h6 "foo")) (parse-wikimarkup "h6. foo")))
  ;;
  ;; Heading with multiple words
  (fiveam:is (equal '((:h1 "foo bar")) (parse-wikimarkup "h1. foo bar")))
  ;;
  ;; More complex lines
  (fiveam:is (equal '((:h1 "foo " (:i "wibble") " bar")) (parse-wikimarkup "h1. foo _wibble_ bar")))
  ;;
  ;; FIXME
  ;; There shouldn't be a 'nil in the second spot, but I've yet to figure
  ;; out how to eliminate it without introducing other bugs.
  (fiveam:is (equal '((:h3 nil (:b "foo") " blah " (:i "meh") " wonkity wonk"))
                    (parse-wikimarkup "h3. *foo* blah _meh_ wonkity wonk")))
  ;;
  ;; Unordered lists
  ;;
  ;; Single list-item
  (fiveam:is (equal '((:ul (:li "foo")))
                    (parse-wikimarkup "- foo")))
  ;; Two consecutive list-items
  (fiveam:is (equal '((:ul (:li "foo") (:li "bar")))
                    (parse-wikimarkup "- foo
- bar")))
  ;; Two consecutive list-items with the first item starting with italic text
  ;; FIXME: I have NFI why the 'nil is in there
  (fiveam:is (equal '((:ul (:li nil (:i "foo") " quux") (:li "bar")))
                    (parse-wikimarkup "- _foo_ quux
- bar")))
  ;; Two consecutive list-items with the first item ending with italic text
  ;; FIXME: I have NFI why the 'nil is in there
  (fiveam:is (equal '((:ul (:li "foo " (:i "quux") nil) (:li "bar")))
                    (parse-wikimarkup "- foo _quux_
- bar")))
  ;; Two consecutive list-items with the first item including italic text
  ;; FIXME: I have NFI why the 'nil is in there
  (fiveam:is (equal '((:ul (:li "foo " (:i "quux") " baz") (:li "bar")))
                    (parse-wikimarkup "- foo _quux_ baz
- bar")))
  ;;
  ;; Two consecutive list-items with the second item starting with italic text
  ;; FIXME: I have NFI why the 'nil is in there
  (fiveam:is (equal '((:ul (:li "foo") (:li nil (:i "bar") " quux")))
                    (parse-wikimarkup "- foo
- _bar_ quux")))
  ;; Two consecutive list-items with the second item ending with italic text
  ;; FIXME: I have NFI why the 'nil is in there
  (fiveam:is (equal '((:ul (:li "bar") (:li "foo " (:i "quux") nil)))
                    (parse-wikimarkup "- bar
- foo _quux_")))
  ;; Two consecutive list-items with the second item including italic text
  ;; FIXME: I have NFI why the 'nil is in there
  (fiveam:is (equal '((:ul (:li "bar") (:li "foo " (:i "quux") " baz")))
                    (parse-wikimarkup "- bar
- foo _quux_ baz")))
  ;;
  ;; Double-newlines
(fiveam:is (equal '("foo" (:BR) "bar")
                  (parse-wikimarkup "foo

bar")))
  ;; Nested unordered lists
  ;; Nested list-item at the start of the list
  (fiveam:is
    (equal '((:ul (:li "foo" (:ul (:li "bar")))))
           (parse-wikimarkup "- foo
-- bar")))
  ;; Nested list-item at the start of a two-item list
  (fiveam:is (equal '((:ul
                        (:li "foo"
                             (:ul (:li "bar")))
                        (:li "baz")))
                    (parse-wikimarkup "- foo
-- bar
- baz")))
;; Two nested list-items at the start of a two-item list
(fiveam:is (equal '((:ul
                      (:li "foo"
                           (:ul (:li "bar")
                                (:li "quux")))
                      (:li "baz")))
                  (parse-wikimarkup "- foo
-- bar
-- quux
- baz")))
;; Two nested list-items at the start of a two-item list,
;; with italic text beginning the first nested item
(fiveam:is (equal '((:ul
                      (:li "foo"
                           (:ul (:li nil (:i "bar") " baz")
                                (:li "quux")))
                      (:li "baz")))
                  (parse-wikimarkup "- foo
-- _bar_ baz
-- quux
- baz")))
;;
;; Two nested list-items at the start of a two-item list,
;; with italic text ending the first nested item
(fiveam:is (equal '((:ul
                      (:li "foo"
                           (:ul (:li "bar " (:i "baz") nil)
                                (:li "quux")))
                      (:li "baz")))
                  (parse-wikimarkup "- foo
-- bar _baz_
-- quux
- baz")))
;;
;; Two nested list-items at the start of a two-item list,
;; with italic text in the middle of the first nested item
(fiveam:is (equal '((:ul
                      (:li "foo"
                           (:ul (:li "bar " (:i "baz") " gronk")
                                (:li "quux")))
                      (:li "baz")))
                  (parse-wikimarkup "- foo
-- bar _baz_ gronk
-- quux
- baz")))
;;
;; Two nested list-items at the start of a two-item list,
;; with italic text beginning the second nested item
(fiveam:is (equal '((:ul
                      (:li "foo"
                           (:ul (:li "quux")
                                (:li nil (:i "bar") " baz")))
                      (:li "baz")))
                  (parse-wikimarkup "- foo
-- quux
-- _bar_ baz
- baz")))
;;
;; Two nested list-items at the start of a two-item list,
;; with italic text ending the second nested item
(fiveam:is (equal '((:ul
                      (:li "foo"
                           (:ul (:li "quux")
                                (:li "baz " (:i "bar") nil)))
                      (:li "baz")))
                  (parse-wikimarkup "- foo
-- quux
-- baz _bar_
- baz")))

;;
;; Two nested list-items at the start of a two-item list,
;; with italic text in the middle of the second nested item
(fiveam:is (equal '((:ul
                      (:li "foo"
                           (:ul (:li "quux")
                                (:li "bar " (:i "baz") " gronk")))
                      (:li "baz")))
                  (parse-wikimarkup "- foo
-- quux
-- bar _baz_ gronk
- baz")))
;;
;;
;; Macros 
;; What we _should_ get
(fiveam:is (equal '((:pre "bar"))
                  (parse-wikimarkup "{pre}bar{/pre}")))
;; Forgot the leading slash on the close-tag
(fiveam:is (equal '((:pre "bar{pre}"))
                  (parse-wikimarkup "{pre}bar{pre}")))
;; Typo giving a mismatched close tag
(fiveam:is (equal '((:pre "bar{/pree}"))
                  (parse-wikimarkup "{pre}bar{/pree}")))
;; End-of-string before the closing tag appears
(fiveam:is (equal '((:pre "bar"))
                  (parse-wikimarkup "{pre}bar")))
;; End-of-string before the opening tag completes
(fiveam:is (equal '("{pre")
                  (parse-wikimarkup "{pre")))
;; End-of-string before the closing tag completes
(fiveam:is (equal '((:pre "bar{/pre"))
                  (parse-wikimarkup "{pre}bar{/pre")))
;; Mid-string
(fiveam:is (equal '("Blah " (:code "content") NIL)
                  (parse-wikimarkup "Blah {code}content{/code}"))))
