#lang racket
(provide (all-defined-out))
(define-syntax-rule (add1! x) (set! x (add1 x)))