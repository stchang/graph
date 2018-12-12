#lang racket/base
(require racket/set)
(provide (all-defined-out))

;; An AdjacencyList is a [MutableHashOf Vertex -> [Setof Vertex]]
;;   and is used as the internal graph representation

;; ----------------------------------------------------------------------------
;; Internal graph functions (operates on the hash table)

;; (internal graph functions and names have a @ suffix)

(define (add-edge@ adj u v) (hash-update! adj u (λ (vs) (set-add vs v)) (set)))
(define (add-vertex@ adj v) (hash-update! adj v (λ (vs) vs) (set)))
(define (remove-edge@ adj u v) (hash-update! adj u (λ (vs) (set-remove vs v))))
(define (remove-vertex@ adj v) 
  ;; remove the vertex as a key in adj
  (hash-remove! adj v)
  ;; remove the vertex from the values of other keys
  (for ([(u vs) (in-hash adj)]) 
    (hash-set! adj u (set-remove vs v))))
(define (rename-vertex@ adj old new)
  (hash-set! adj new (hash-ref adj old))
  (hash-remove! adj old)
  (for ([(u vs) (in-hash adj)] #:when (set-member? vs old))
    (hash-set! adj u (set-add (set-remove vs old) new))))