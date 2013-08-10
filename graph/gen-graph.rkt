#lang racket
(require racket/generic)
(provide (all-defined-out)) ; not enough to only provide gen:graph
;; generic graph interface
(define-generics graph
  (in-vertices graph)
  (in-neighbors graph v)
  (edge-weight graph u v)
  (add-directed-edge! graph u v [weight])
  (add-edge! graph u v [weight])
  (add-vertex! graph v))