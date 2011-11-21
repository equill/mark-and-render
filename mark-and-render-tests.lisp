(in-package :mark-and-render-tests)

;;; Short instructions:
;;; (asdf:load-system :mark-and-render-tests)
;;; (in-package :mark-and-render-tests)
;;; (run! 'm-a-r)


(def-suite m-a-r)
(in-suite m-a-r)

(test (parse-checks)
      ;; Regular, plain text
      (is (equal '("foo") (parse-wikimarkup "foo")))
      (is (equal '("foo bar") (parse-wikimarkup "foo bar")))
      ;;
      ;; Italic at start of line
      (is (equal '((:i "foo")) (parse-wikimarkup "_foo_")))
      ;;
      ;; Escaped italic at start of line
      (is (equal '("_foo_") (parse-wikimarkup "\\_foo\\_")))
      ;;
      ;; Bold at start of line
      (is (equal '((:b "foo")) (parse-wikimarkup "*foo*")))
      ;;
      ;; Escaped bold at start of line
      (is (equal '("*foo*") (parse-wikimarkup "\\*foo\\*")))
      ;;
      ;; Leading italic followed by regular text
      (is (equal '((:i "foo") " bar") (parse-wikimarkup "_foo_ bar")))
      ;;
      ;; Leading italic followed by regular text
      (is (equal '((:b "foo") " bar") (parse-wikimarkup "*foo* bar")))
      ;;
      ;; Escaped italic in mid-line
      (is (equal '("bar _foo_") (parse-wikimarkup "bar \\_foo\\_")))
      ;;
      ;; Escaped bold in mid-line
      (is (equal '("bar *foo*") (parse-wikimarkup "bar \\*foo\\*")))
      ;;
      ;; FIXME
      ;; The next two shouldn't have the trailing 'nil, but I've
      ;; yet to figure out how to eliminate it.
      ;; Leading bold with mid-line italic
      (is (equal '((:b "foo") " " (:i "bar") nil) (parse-wikimarkup "*foo* _bar_")))
      ;;
      ;; Leading italic with mid-line bold
      (is (equal '((:i "foo") " " (:b "bar") nil) (parse-wikimarkup "_foo_ *bar*")))
      ;;
      ;; Leading bold with mid-line italic and trailing normal text
      (is (equal '((:b "foo") " " (:i "bar") " baz") (parse-wikimarkup "*foo* _bar_ baz")))
      ;;
      ;; Leading bold with mid-line italic and unspaced trailing normal text
      (is (equal '((:b "foo") " " (:i "bar") "baz") (parse-wikimarkup "*foo* _bar_baz")))
      ;;
      ;; Headings
      (is (equal '((:h1 "foo")) (parse-wikimarkup "h1. foo")))
      (is (equal '((:h2 "foo")) (parse-wikimarkup "h2. foo")))
      (is (equal '((:h3 "foo")) (parse-wikimarkup "h3. foo")))
      (is (equal '((:h4 "foo")) (parse-wikimarkup "h4. foo")))
      (is (equal '((:h5 "foo")) (parse-wikimarkup "h5. foo")))
      (is (equal '((:h6 "foo")) (parse-wikimarkup "h6. foo")))
      ;;
      ;; Heading with multiple words
      (is (equal '((:h1 "foo bar")) (parse-wikimarkup "h1. foo bar")))
      ;;
      ;; More complex lines
      (is (equal '((:h1 "foo " (:i "wibble") " bar")) (parse-wikimarkup "h1. foo _wibble_ bar")))
      ;;
      ;; FIXME
      ;; There shouldn't be a 'nil in the second spot, but I've yet to figure
      ;; out how to eliminate it without introducing other bugs.
      (is (equal '((:h3 nil (:b "foo") " blah " (:i "meh") " wonkity wonk"))
                 (parse-wikimarkup "h3. *foo* blah _meh_ wonkity wonk")))
      (is (equal '((:ul (:li "foo")))
                 (parse-wikimarkup "- foo")))
      (is (equal '((:ul (:li "foo") (:li "bar")))
                 (parse-wikimarkup "- foo
- bar"))))

#+(or)
(run! 'm-a-r)
