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

; should clean this up

(def parse-attrs (t as/body acc)
  (if no.as/body
       (apply empty-tag t rev.acc)
      (or (acons car.as/body)
          (isa car.as/body 'string))
       (apply tag t rev.acc as/body)
       (let (attr val . rest) as/body
         (parse-attrs t rest (cons val (cons attr acc))))))

(def htmlf (s)
  (if no.s                   nil
      atom.s                 pr.s
      (caris s 'arc)         (apply eval cdr.s)
      (html-macs* car.s)     (apply (html-macs* car.s) cdr.s)
                             (parse-attrs car.s cdr.s nil)))

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
    (ero (string "html test " name " failed"))))

(html-test "#1" '(foo) "<foo/>")
(html-test "#2" '(foo a 1) "<foo a=\"1\"/>")
(html-test "#3" '(foo a 1 "bar") "<foo a=\"1\">bar</foo>")
(html-test "#4" '(foo a 1 (bar "baz")) "<foo a=\"1\"><bar>baz</bar></foo>")

;; lib

(html-mac link (text (o dest text))
  `((a href ,dest) ,text))

