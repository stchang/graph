#lang racket
(require racket/stxparam #;syntax/parse/experimental/template)
(provide (all-defined-out))

(define-syntax-rule (add1! x) (set! x (add1 x)))

(define-syntax-parameter $v (syntax-rules ()))
(define-syntax-parameter $from (syntax-rules ()))
(define-syntax-parameter $to (syntax-rules ()))
(define-syntax-parameter $discovered? (syntax-rules ()))
(define-syntax-parameter $seen? (syntax-rules ()))
(define-syntax-parameter $visited? (syntax-rules ()))
(define broke? (make-parameter #f))
(define-syntax-parameter $broke? (syntax-rules ()))


(define WHITE 'white)
(define BLACK 'black)
(define GRAY 'gray)

(define (white? c) (eq? WHITE c))
(define (gray? c) (eq? GRAY c))
(define (black? c) (eq? BLACK c))
