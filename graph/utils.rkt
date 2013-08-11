#lang racket
(provide (all-defined-out))

(define-syntax-rule (add1! x) (set! x (add1 x)))


(define WHITE 'white)
(define BLACK 'black)
(define GRAY 'gray)

(define (white? c) (eq? WHITE c))
(define (gray? c) (eq? GRAY c))
(define (black? c) (eq? BLACK c))