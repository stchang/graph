#lang racket
(require racket/generic)
(provide (all-defined-out))
(define-generics queue
  (enqueue! queue x)
  (peek queue)
  (dequeue! queue)
  (empty? queue)
  ;; restore queue invariants 
  ;; (ie like for a min/max priority queue dependent on external keys)
  (restore queue))