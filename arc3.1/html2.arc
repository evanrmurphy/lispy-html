;;; html2.arc

;; utils

(def cdar (xs) (cdr (car xs)))

;; main

(let nestlev 0

  (def q ()
    (case nestlev
      0  (pr #\")
      1  (pr "&quot;")
         (ero "maximum html nesting level exceeded")))

  (def open-q () (q) ++.nestlev)

  (def close-q () --.nestlev (q)))

(mac w/quotes body
  `(do (open-q)
       ,@body
       (close-q)))

(def attrs (as)
  (each a pair.as
    (pr #\ car.a #\=)
    (w/quotes
      (htmlf cadr.a))))

(def start-tag (t . as)
  (pr #\< t)
  attrs.as
  (pr #\>))

(def end-tag (t)
  (pr #\< #\/ t #\>))

(def empty-tag (t . as)
  (pr #\< t)
  attrs.as
  (pr #\/ #\>))

(def tag (t as . body)
  (apply start-tag t as)
  (if (acons car.body)
       (apply htmlfs body)
       (apply pr body))
  (end-tag t))

(= html-macs* (table))

(mac html-mac (name args . body)
  `(= (html-macs* ',name) (fn ,args (htmlf ,@body))))

(def htmlf (s)
  (if no.s                   nil
      atom.s                 pr.s
      (caris s 'arc)         (apply eval cdr.s)
      (html-macs* car.s)     (apply (html-macs* car.s) cdr.s)
      (acons car.s)          (if (no cdr.s)
                                  (apply empty-tag caar.s cdar.s)
                                  (apply tag caar.s cdar.s cdr.s))
                             (if (no cdr.s)
                                  (apply empty-tag car.s nil)
                                  (apply tag car.s nil cdr.s))))

(def htmlfs args
  (each a args
    htmlf.a))

; bug: repl seems to be blocking stderr

(def html-repl ()
  ((afn ()
     (pr "html> ")
     (let that (read)
       (unless (iso that '(quit))
         (htmlfs that)
         (prn)
         (self))))))

(def html args
  (if (no args)
       (html-repl)
       (do (apply htmlfs args)
           ; just puts return value on the next line
           (prn))))

;; tests

(def html-test (name x expected)
  (unless (iso (tostring:html x)
               (string expected #\newline))
    (err (string "html test " name " failed"))))

(html-test "#1" '(foo) "<foo/>")
(html-test "#2" '(bar) "<bar/>")
(html-test "#3" '(foo (bar)) "<foo><bar/></foo>")
(html-test "#4" '((foo a 1 b 2)) "<foo a=\"1\" b=\"2\"/>")

; might want it to work this way

; (html-test "#" '(foo) "<foo></foo>")
; (html-test "#" '(foo a 1) "<foo a=\"1\"></foo>")
; (html-test "#" '(foo a 1 "bar") "<foo a=\"1\">bar</foo>")
; (html-test "#" '(foo a 1 (bar "baz")) "<foo a=\"1\"><bar>baz</bar></foo>")

;; lib

(html-mac link (text (o dest text))
  `((a href ,dest) ,text))

