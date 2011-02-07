(def cdar (xs)
  (cdr (car xs)))

(let html-nestlev 0

  (def html-q ()
    (if (is html-nestlev 0)
        (pr #\")
        (pr "&quot;"))
    
    ; way to warn not on stdout?

    ;(if (> html-nestlev 1)
    ;    (warn "maximum html nest level exceeded")) 

    )

  (def html-openq ()
    (html-q)
    ++.html-nestlev)

  (def html-closeq ()
    --.html-nestlev
    (html-q)))

(mac html-w/quotes body
  `(do (html-openq)
       ,@body
       (html-closeq)))


(def attrs (as)
  (each a pair.as
    (pr #\ car.a #\=)
    (html-w/quotes
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

(html-mac link (text (o dest text))
  `((a href ,dest) ,text))

(def htmlf (s)
  (if no.s                   nil
      ;(isa s 'string)        (html-w/quotes pr.s)
      atom.s                 pr.s
      (caris s 'arc)         (apply eval cdr.s)
      (caris s 'js)          (apply jsfs cdr.s)
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

(mac html args
  `(apply htmlfs ',args))
