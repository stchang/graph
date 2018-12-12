#lang racket/base
(require racket/stxparam racket/unsafe/ops)
(provide (all-defined-out))

(define-syntax-rule (unsafe-add1 x) (unsafe-fx+ x 1))
(define-syntax-rule (unsafe-add1! x) (set! x (unsafe-add1 x)))

(define-syntax-parameter $v (syntax-rules ()))
(define-syntax-parameter $from (syntax-rules ()))
(define-syntax-parameter $to (syntax-rules ()))
(define-syntax-parameter $discovered? (syntax-rules ()))
(define-syntax-parameter $seen? (syntax-rules ()))
(define-syntax-parameter $visited? (syntax-rules ()))
(define-syntax-parameter $broke? (syntax-rules ()))
(define-syntax-parameter $acc (syntax-rules ()))


(define WHITE 'white)
(define BLACK 'black)
(define GRAY 'gray)

(define (white? c) (eq? WHITE c))
(define (gray? c) (eq? GRAY c))
(define (black? c) (eq? BLACK c))
