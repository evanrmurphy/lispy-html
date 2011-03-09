;;; html2.arc

;; utils

(def cdar (xs) (cdr (car xs)))

;; core

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
      (caris s 'mac)         (eval `(html-mac ,@cdr.s))
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

;; tests and lib

(def html-test (name x expected)
  (unless (iso (tostring:html x)
               (string expected #\newline))
    (ero (string "html test " name " failed"))))

(html-test "#1" '(foo) "<foo/>")
(html-test "#2" '(foo a 1) "<foo a=\"1\"/>")
(html-test "#3" '(foo a 1 "bar") "<foo a=\"1\">bar</foo>")
(html-test "#4" '(foo a 1 (bar "baz")) "<foo a=\"1\"><bar>baz</bar></foo>")

(html-mac link (text (o dest text))
  `(a href ,dest ,text))

(html-test "#5" '(link "foo") "<a href=\"foo\">foo</a>")
(html-test "#6" '(link "foo" "http://bar.com") "<a href=\"http://bar.com\">foo</a>")

(html-mac page args
  `(html (body ,@args)))

(html-test "#7" '(page "foo") "<html><body>foo</body></html>")

(html-mac trtd body
  `(tr (td ,@body)))

(html-test "#8" '(trtd "foo") "<tr><td>foo</td></tr>")

(html-mac list args
  `(ul ,@(map (fn (_) `(li ,_)) args)))

(html-test "#9" '(list "foo") "<ul><li>foo</li></ul>")
(html-test "#10" '(list "foo" "bar") "<ul><li>foo</li><li>bar</li></ul>")

(html-mac row args
  `(tr ,@(map (fn (_) `(td ,_)) args)))

(html-test "#11" '(row "foo") "<tr><td>foo</td></tr>")
(html-test "#12" '(row "foo" "bar") "<tr><td>foo</td><td>bar</td></tr>")

(html-mac tab body
  `(table border 0 ,@body))

(html-test "#13" '(tab "foo") "<table border=\"0\">foo</table>")
(html-test "#14" '(tab (tr (td "foo") (td "bar"))) "<table border=\"0\"><tr><td>foo</td><td>bar</td></tr></table>")

(html-mac table-of (b p s . body)
  `(table border ,b cellpadding ,p cellspacing ,s
     ,@body))

(html-test "#15" '(table-of 0 2 0 "foo") "<table border=\"0\" cellpadding=\"2\" cellspacing=\"0\">foo</table>")

(html-mac zerotable body
  `(table-of 0 0 0
     ,@body))

(html-test "#16" '(zerotable "foo") "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\">foo</table>")
