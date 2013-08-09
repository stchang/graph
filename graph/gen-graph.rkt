#lang racket
(require racket/generic)
(provide (all-defined-out)) ; not enough to only provide gen:graph
;; generic graph interface
(define-generics graph
  (in-vertices graph)
  (in-neighbors graph v)
  (in-edges graph)
  (edge-weight graph u v))