#lang racket
(require graph rackunit)

;; example from kmicinski on github 2019-07-20

;; Each vertex is a pair via a struct
(struct vtx (tag contents) #:transparent)

;; List of edges in an undirected graph
(define edges
  (list
   (list (vtx 0 (set 3)) (vtx 1 (set 3 2)))
   (list (vtx 0 (set 0)) (vtx 1 (set 3 0)))
   (list (vtx 0 (set 1)) (vtx 1 (set 1 2)))
   (list (vtx 0 (set 1)) (vtx 1 (set 1 3)))
   (list (vtx 0 (set 3 2)) (vtx 1 (set 3 0 2)))
   (list (vtx 0 (set 0)) (vtx 1 (set 0 2)))
   (list (vtx 0 (set 3)) (vtx 1 (set 1 3)))
   (list (vtx 0 (set 3)) (vtx 1 (set 3 0)))
   (list (vtx 0 (set 1)) (vtx 1 (set 1 3 0)))
   (list (vtx 0 (set 2)) (vtx 1 (set 3 0 2)))
   (list (vtx 0 (set 3 0)) (vtx 1 (set 3 0 2)))
   (list (vtx 0 (set 2)) (vtx 1 (set 0 2)))
   (list (vtx 0 (set 3)) (vtx 1 (set 1 3 0)))
   (list (vtx 0 (set 0)) (vtx 1 (set 3 0 2)))
   (list (vtx 0 (set 1 3)) (vtx 1 (set 1 3 0)))
   (list (vtx 0 (set 2)) (vtx 1 (set 3 2)))
   (list (vtx 0 (set 3)) (vtx 1 (set 3 0 2)))
   (list (vtx 0 (set 2)) (vtx 1 (set 1 2)))
   (list (vtx 0 (set 3 0)) (vtx 1 (set 1 3 0)))
   (list (vtx 0 (set 0)) (vtx 1 (set 1 3 0)))
   (list (vtx 0 (set 0 2)) (vtx 1 (set 3 0 2)))))

(define G (undirected-graph edges))
(define L-R (bipartite? G))
(check-not-false L-R)
;; Perform maximum bipartite matching.
(define matching (maximum-bipartite-matching G))
;; should be complete matching
(check-equal? (length matching) 6 "check number of pairings")
(check-true
 (set-empty?
  (set-intersect (map first matching) (map second matching))))

;; check no dups
(check-equal? (set-count (map first matching))
              (set-count (list->set (map first matching)))
              "max matching: check no dups 1")
(check-equal? (set-count (map second matching))
              (set-count (list->set (map second matching)))
              "max matching: check no dups 2")

(check-true (subset? (map first matching) (second L-R)))
(check-true (subset? (map second matching) (first L-R)))
