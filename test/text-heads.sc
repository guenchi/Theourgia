#!r6rs
(import (chezscheme) (theourgia regex) (theourgia languages) (theourgia text-code))
(define bad 0)
(define (want name got expected)
  (if (equal? got expected) (printf "ok ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" name got expected))))
(define expected
  '(("scheme" "f" "x" "m") ("javascript" "f" "C" "inc") ("typescript" "Shape" "Count" "f")
    ("python" "f" "C" "answer") ("go" "f" "Count" "M") ("rust" "f" "Thing" "Thing")
    ("c" "f" "g" "Point") ("java" "C" "I" "E") ("shell" "f" "g" "h")
    ("markdown" "Alpha" "Beta" "Gamma") ("chez" "f" "x" "m")))
(for-each (lambda (entry)
            (let ((name (language-property entry 'lang #f)))
              (want (string-append "CT-05 " name " three definition heads")
                    (map (lambda (s) (definition-name entry s)) (language-property entry 'name-vectors '()))
                    (cdr (assoc name expected))))) (language-table))
(define scheme (language-for-name 'scheme))
(want "CT-14 dangerous numeric text remains only text" (definition-name scheme "#e1e99999999") #f)
(want "CT-14 numeric-looking binding does not enter a numeric reader" (definition-name scheme "(define #e1e99999999 1)") "#e1e99999999")
(want "CT-05 unknown head has no guessed name" (definition-name scheme "(magic x y)") #f)
(want "CT-05 anonymous head has no guessed name" (definition-name (language-for-name 'javascript) "function () {}") #f)
(want "CT-05 unsupported Unicode name cannot become an ASCII prefix"
      (definition-name (language-for-name 'javascript) "function café() {}") #f)
(want "CT-06 comments remain in src while supplying doc"
      (text-properties (language-for-name 'python) (string->utf8 "# docs\r\ndef f():\n  pass\n"))
      (list "f" (string->utf8 "# docs\r\n")))
(want "CT-06 an internal docstring is not leading documentation"
      (text-properties (language-for-name 'python) (string->utf8 "def f():\n  \"docs\"\n")) '("f" #vu8()))
(want "CT-17 shebang and cookie keep their exact prefix"
      (source-prefix-size (string->utf8 "#!/bin/python\r\n# coding: utf-8\nprint(1)")) 31)
(want "CT-02 unknown extension has no language" (language-for-path "one.toy") #f)
(register-language! '((lang "toy") (extensions ("toy")) (line-comment "//")
                      (def-heads ("^thing ([A-Za-z]+)")) (name-capture 1)))
(want "CT-02 one data row adds a language and its name rule"
      (definition-name (language-for-path "one.toy") "thing Example") "Example")
(want "CT-04 regex work is bounded"
      (guard (e (#t 'bounded)) (regex-match (regex-compile "^(a+)+b$") (make-string 32 #\a)) 'unbounded) 'bounded)
(printf "~a failures\ntext-heads complete\n" bad)
(exit (if (zero? bad) 0 1))
