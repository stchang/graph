#lang setup/infotab

(define compile-omit-paths '("graph"))

(define test-timeouts
  '(("graph/timing-test-in-neighbors.rkt" 360)
    ("graph/timing-test-scc.rkt" 360)))
