#lang racket

(provide (all-defined-out))

(define (lists->sets lsts) (apply set (map (λ (lst) (apply set lst)) lsts)))
