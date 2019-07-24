#lang setup/infotab

(define compile-omit-paths '("tests"))

(define test-timeouts
  '(("graph/timing-test-in-neighbors.rkt" 180)
    ("graph/timing-test-scc.rkt" 180)))
