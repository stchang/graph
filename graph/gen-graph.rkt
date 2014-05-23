#lang racket
(require racket/generic)
(provide (all-defined-out)) ; not enough to only provide gen:graph
;; generic graph interface
(define-generics graph
  (has-vertex? graph v)
  (has-edge? graph u v)
  (add-edge! graph u v [weight])
  (add-directed-edge! graph u v [weight])
  (remove-edge! graph u v)
  (remove-directed-edge! graph u v)
  (add-vertex! graph v)
  (remove-vertex! graph v)
  (in-vertices graph)
  (in-neighbors graph v)
  (in-edges graph)
  (edge-weight graph u v)
  (transpose graph)
  (graph-copy graph))
