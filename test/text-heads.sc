#!r6rs
(import (chezscheme) (theourgia regex) (theourgia languages) (theourgia text-code))
(define bad 0)
(define (want name got expected)
  (if (equal? got expected) (printf "ok ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" name got expected))))
(define expected
  '(("scheme" "f" "x" "m") ("javascript" "f" "C" "inc") ("typescript" "Shape" "Count" "f")
    ("typescriptreact" "Props" "Screen" "Row") ("javascriptreact" "Screen" "Panel" "Row")
    ("python" "f" "C" "answer") ("go" "f" "Count" "M") ("rust" "f" "Thing" "Thing")
    ("c" "f" "g" "Point") ("java" "C" "I" "E") ("shell" "f" "g" "h") ("perl" "f" "Foo::bar" "Foo")
    ("markdown" "Alpha" "Beta" "Gamma") ("chez" "f" "x" "m")))
(for-each (lambda (entry)
            (let ((name (language-property entry 'lang #f)))
              (want (string-append "CT-05 " name " three definition heads")
                    (map (lambda (s) (definition-name entry s)) (language-property entry 'name-vectors '()))
                    (cdr (assoc name expected))))) (language-table))
;; THE PERL ENTRY, asked by name: the loop above asks only the entries the
;; table has, so it cannot be red on a table without perl. A head is a sub or
;; a package at column 0, named by its spelling, `::` included; an indented
;; sub, a sub stored in a variable and an anonymous sub are not heads.
(define perl (language-for-name 'perl))
(want "CT-05 perl three definition heads: a sub, a qualified sub, a package"
      (and perl (map (lambda (s) (definition-name perl s)) (language-property perl 'name-vectors '())))
      '("f" "Foo::bar" "Foo"))
(want "CT-05 perl heads: a one-line sub, a forward declaration and a package block are named; an indented sub, `my $sub = sub`, an anonymous sub and `sub` alone are not"
      (and perl (map (lambda (s) (definition-name perl s))
                     '("sub three { 3 }" "sub four;" "package Foo {" "  sub f {" "my $sub = sub { 1 };" "sub { 1 }" "sub")))
      '("three" "four" "Foo" #f #f #f #f))
(want "CT-05 perl: .pl, .pm and .t are perl files, the entry has no runner and no suggest-only profile, and its line-suggest stops at __END__ and __DATA__"
      (list (map (lambda (p) (let ((e (language-for-path p))) (and e (language-property e 'lang #f)))) '("a.pl" "a.pm" "a.t"))
            (and perl (language-runner perl)) (and perl (language-property perl 'suggest-only #f))
            (and perl (language-property perl 'line-suggest #f)))
      '(("perl" "perl" "perl") #f #f ((stop-lines ("__END__" "__DATA__")))))
(want "CT-05 shell keeps its suggest-only profile and gains an empty line-suggest"
      (let ((sh (language-for-name 'shell)))
        (list (and (language-property sh 'suggest-only #f) #t) (language-property sh 'line-suggest #f)))
      '(#t ()))
(define scheme (language-for-name 'scheme))
(want "CT-14 dangerous numeric text remains only text" (definition-name scheme "#e1e99999999") #f)
(want "CT-14 numeric-looking binding does not enter a numeric reader" (definition-name scheme "(define #e1e99999999 1)") "#e1e99999999")
(want "CT-05 unknown head has no guessed name" (definition-name scheme "(magic x y)") #f)
(want "CT-05 anonymous head has no guessed name" (definition-name (language-for-name 'javascript) "function () {}") #f)
;; THE ECMASCRIPT FUNCTION HEAD IN EVERY ENTRY THAT READS JAVASCRIPT OR
;; TYPESCRIPT: exported as the default, async, and both, each names f, in
;; the parents as in the jsx entries.
(for-each
  (lambda (lang)
    (want (string-append "CT-05 " (symbol->string lang) " export default, export async, async and export default async function heads")
          (map (lambda (s) (definition-name (language-for-name lang) s))
               '("export default function f() {}" "export async function f() {}"
                 "async function f() {}" "export default async function f() {}"))
          '("f" "f" "f" "f")))
  '(javascript typescript typescriptreact javascriptreact))
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
