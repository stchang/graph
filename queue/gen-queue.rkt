#lang racket/base
(require racket/generic)
(provide (all-defined-out))
(define-generics queue
  (enqueue! queue x)
  (peek queue)
  (dequeue! queue)
  (empty? queue)
  (in-queue queue))