#lang racket

(require "../graph-weighted.rkt"
         "../graph-unweighted.rkt"
         "../graph-fns-singlesource-shortestpaths.rkt"
         "../graph-fns-minspantree.rkt"
         "../graph-fns-basic.rkt"
         "../gen-graph.rkt"
         "test-utils.rkt")

(require rackunit)

;; limits determined running cmd line racket, on 2.40Ghz quad-core

;; large (200 vertices) graph (unweighted) to check timing
;; from Coursera Stanford Algorithms course
(define g/cours
  (with-input-from-file "kargerMinCut.txt"
    (lambda ()
      (mk-unweighted-graph/adj
        (for/list ([vs (in-lines)])
          (map string->number (string-split vs "\t")))))))

;; speed up these tests by not testing every v

;; most are < 10 ms; v=173 is ~11ms; v=174 is ~85ms
(define DIJ-TIME-LIMIT 100) ; ms

;; dijkstra
(for ([v (in-vertices g/cours)] #:when (even? v))
  (define-values (res cpu real gc) (time-apply dijkstra (list g/cours v)))
  (check-true (< real DIJ-TIME-LIMIT)))

;; most are < 350ms; v=67: ~362ms; v=75: ~378ms; v=135: ~417ms; v=59: ~380ms
(define BELL-TIME-LIMIT 500) ; ms

;; bellman-ford
(for ([v (in-vertices g/cours)] #:when (zero? (modulo v 5)))
  (define-values (res cpu real gc) (time-apply bellman-ford (list g/cours v)))
  (check-true (< real BELL-TIME-LIMIT)))

(define KRUSKAL-TIME-LIMIT 40) ; ms

;; mst-kruskal
(let-values ([(res cpu real gc) (time-apply mst-kruskal (list g/cours))])
  (when (> real KRUSKAL-TIME-LIMIT) (displayln real))
  (check-true (< real KRUSKAL-TIME-LIMIT)))

;; most are ~3ms; v=71: ~7ms
(define PRIM-TIME-LIMIT 10) ; ms

;; mst-prim
(for ([v (in-vertices g/cours)] #:when (odd? v))
  (define-values (res cpu real gc) (time-apply mst-prim (list g/cours v)))
;  (when (> real PRIM-TIME-LIMIT) (displayln real) (displayln v))
  (check-true (< real PRIM-TIME-LIMIT)))
