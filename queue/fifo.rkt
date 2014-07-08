#lang racket/base
(require "gen-queue.rkt")
(require (prefix-in r: data/queue))
(provide (all-defined-out))

(struct fifo (elements)
  #:methods gen:queue
  [(define (enqueue! ff x) (r:enqueue! (fifo-elements ff) x))
   (define (peek ff) 
     (define q (fifo-elements ff))
     (define x (r:dequeue! q))
     (begin0 x (r:enqueue-front! q x)))
   (define (dequeue! ff) (r:dequeue! (fifo-elements ff)))
   (define (empty? ff) (r:queue-empty? (fifo-elements ff)))
   (define (in-queue ff) (r:in-queue (fifo-elements ff)))])

(define (mk-empty-fifo) (fifo (r:make-queue)))