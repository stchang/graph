#lang setup/infotab
(define scribblings '(("graph.scrbl" ())))
(define compile-omit-paths '("tests"))
(define test-timeouts
  '(("tests/timing-test-in-neighbors.rkt" 180)
    ("tests/timing-test-scc.rkt" 180)))
