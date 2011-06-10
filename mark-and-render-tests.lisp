(in-package :mark-and-render-tests)

(use-package :eos)

(defmacro with-test-results ((results test-name) &body body)
  `(let ((,results (with-*test-dribble* nil (run ',test-name))))
     ,@body))

(def-suite m-a-r)
(in-suite m-a-r)

(test (parse-checks)
      (is (equal '("foo") (parse-wikimarkup "foo")))
      (is (equal '((:i "foo") nil) (parse-wikimarkup "_foo_")))
      (is (equal '((:b "foo") nil) (parse-wikimarkup "*foo*")))
      (is (equal '(:h1 "foo") (parse-wikimarkup ":h1 foo")))
      (is (equal '(:h2 "foo") (parse-wikimarkup ":h2 foo")))
      (is (equal '(:h3 "foo") (parse-wikimarkup ":h3 foo")))
      (is (equal '(:h4 "foo") (parse-wikimarkup ":h4 foo")))
      (is (equal '(:h5 "foo") (parse-wikimarkup ":h5 foo")))
      (is (equal '(:h6 "foo") (parse-wikimarkup ":h6 foo")))
      (is (equal '(:h1 "foo bar") (parse-wikimarkup ":h1 foo bar")))
      ;(is (equal (parse-wikimarkup ":h3 *foo* blah _meh_ wonkity wonk")) ())
      )

#+(or)
(run! 'm-a-r)
