#lang racket/base

(require "gen-graph.rkt")
(require math/matrix math/array)

(require racket/sequence racket/set racket/unsafe/ops)

;; matrix graph: vertices are 0-based (matrix row/col) numbers
;; The entry in the matrix at row i, col j represents the weight of edge i-j.
;; A non-existent edge is denoted with #f.

(provide mk-matrix-graph matrix-graph?)

(define-syntax-rule (get-matrix g) (unsafe-struct*-ref g 0))

;; A MatrixGraph is a (matrix-graph Matrix)
(struct matrix-graph (matrix)
  #:methods gen:equal+hash
  [(define (equal-proc g1 g2 equal?-recur) 
     (equal?-recur (get-matrix g1) (get-matrix g2)))
   (define (hash-proc g hash-recur) 
     (+ (* 3 (hash-recur (get-matrix g))) (* 5 (hash-recur (get-matrix g)))))
   (define (hash2-proc g hash2-recur)
     (+ (* 6 (hash2-recur (get-matrix g))) (* 7 (hash2-recur (get-matrix g)))))]
  #:methods gen:graph
  [(define (get-vertices g) (sequence->list (in-vertices g)))
   (define (in-vertices g) (in-range (matrix-num-rows (get-matrix g))))
   (define (in-neighbors g v) (in-list (get-neighbors g v)))
   (define (get-neighbors g v) 
     (define m (get-matrix g))
     (for/list ([(x i) (in-indexed (in-array (matrix-row m v)))] #:when x) i))
   (define (vertex=? g u v) (unsafe-fx= u v))
   (define (edge-weight g u v)
     (or (matrix-ref (get-matrix g) u v) +inf.0))
   (define (add-directed-edge! g u v [weight 1])
     (unless (and (has-vertex? g u) (has-vertex? g v))
       (error 'add-directed-edge! "Can't add vertex to matrix graph: ~a ~a." u v))
     (define m (get-matrix g))
     (array-set! m (vector u v) weight))
   (define (add-edge! g u v [weight 1])
     (unless (and (has-vertex? u) (has-vertex? v))
       (error 'add-directed-edge! "Can't add vertex to matrix graph: ~a ~a." u v))
     (define m (get-matrix g))
     (array-set! m u v weight)
     (array-set! m v u weight))
   (define (remove-directed-edge! g u v)
     (define m (get-matrix g))
     (array-set! u v #f))
   (define (remove-edge! g u v)
     (define m (get-matrix g))
     (array-set! u v #f)
     (array-set! v u #f))
   (define (add-vertex! g v) 
     (error 'add-vertex! "Can't add vertex to matrix graph."))
   (define (remove-vertex! g v)
     (define m (get-matrix g))
     (array-slice-set! m (list (list v) (::)) 
                       (vector->array (make-vector (matrix-num-rows m) #f)))
     (array-slice-set! m (list (::) (list v))
                       (vector->array (make-vector (matrix-num-rows m) #f))))
   (define (rename-vertex! g old new) 
     (error 'rename-vertex! "Can't rename vertex in matrix graph."))
   (define (has-vertex? g v) 
     (and (number? v)
          (unsafe-fx< v (matrix-num-rows (get-matrix g)))))
   (define (has-edge? g u v) (and (matrix-ref (get-matrix g) u v) #t))
   ;; returns edges as a sequence
   (define (get-edges g)
     (for*/list ([u (in-vertices g)] [v (in-neighbors g u)]) (list u v)))
   (define (in-edges g) (in-list (get-edges g)))
   (define (graph-copy g)
     (struct-copy matrix-graph g
                  [matrix (mutable-array-copy (get-matrix g))]))
   (define (transpose G)
     (matrix-graph (matrix-transpose (get-matrix G))))])

(define-syntax-rule (mk-matrix-graph rows) 
  (let ([m (matrix rows)])
    (unless (square-matrix? m) (error 'mk-matrix-graph "graph must be a square matrix"))
    (matrix-graph (array->mutable-array m))))
