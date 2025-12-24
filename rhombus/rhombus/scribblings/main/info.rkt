#lang info

(define scribblings
  '(("rhombus.scrbl" (depends-all-main no-depend-on) (language))))

(define main-doc-index "rhombus/scribblings/main")

(define language-family (list (hash 'fam "Rhombus"
                                    'famroot "rhombus"
                                    'describe-doc '(lib "rhombus/scribblings/guide/rhombus-guide.scrbl"))))
