#lang racket
(require "../graph-unweighted.rkt" 
         "../graph-weighted.rkt"
         "../gen-graph.rkt" 
         "../graph-fns-basic.rkt"
         "../graph-fns-singlesource-shortestpaths.rkt"
         "../graph-property.rkt"
         "../utils.rkt"
         "test-utils.rkt")
(require rackunit)

;; adjacency list tests -- Thanks to Scott Klarenbach
(define g-adj1 (mk-unweighted-graph/adj '((a b c) (b c d))))
(check-true (dag? g-adj1))
(define-values (ds-adj1 π-adj1) (bellman-ford g-adj1 'a))
(check-equal? ds-adj1 (make-hash '((d . 2) (c . 1) (a . 0) (b . 1))))
(check-equal? π-adj1 (make-hash '((d . b) (c . a) (a . #f) (b . a))))

(define g-adj2 (mk-unweighted-graph/adj '((a b) (a c) (b c))))
(check-seqs-as-equal-sets? (in-vertices g-adj2) '(a b c))

(check-seqs-as-equal-sets? (in-edges g-adj2)
                                '((a b) (a c) (b c)))

;; bfs tests ------------------------------------------------------------------

;; graph from clrs figure 22.3
(define g22.3
  (mk-unweighted-graph/undirected
   '((r v) (r s) (s w) (w t) (w x) (x t) (t u) (x u) (x y) (y u))))

(check-seqs-as-equal-sets? '(v r s w t x u y) (in-vertices g22.3))

(define-values (d22.3 π22.3) (bfs g22.3 's))

(check-equal? 
 d22.3
 (make-hash '((t . 2) (y . 3) (u . 3) (s . 0) (v . 2) (x . 2) (w . 1) (r . 1))))
(check-equal?
 π22.3
 (make-hash '((t . w) (y . x) (u . x) (s . #f) (v . r) (x . w) (w . s) (r . s))))

(check-equal? '(s r v) (fewest-vertices-path g22.3 's 'v))
(check-equal? '(s w t) (fewest-vertices-path g22.3 's 't))
(check-equal? '(s w x) (fewest-vertices-path g22.3 's 'x))
(check-equal? '(s w x u) (fewest-vertices-path g22.3 's 'u))
(check-equal? '(s w x y) (fewest-vertices-path g22.3 's 'y))

;; same as above, but use mk-undirected-graph constructor
(define g22.3b (mk-undirected-graph 
                '((r v) (r s) (s w) (w t) (w x) (x t) (t u) (x u) (x y) (y u))))

(check-seqs-as-equal-sets? '(v r s w t x u y) (in-vertices g22.3b))

(define-values (d22.3b π22.3b) (bfs g22.3b 's))

(check-equal? 
 d22.3b
 (make-hash '((t . 2) (y . 3) (u . 3) (s . 0) (v . 2) (x . 2) (w . 1) (r . 1))))
(check-equal?
 π22.3b
 (make-hash '((t . w) (y . x) (u . x) (s . #f) (v . r) (x . w) (w . s) (r . s))))

(check-equal? '(s r v) (fewest-vertices-path g22.3b 's 'v))
(check-equal? '(s w t) (fewest-vertices-path g22.3b 's 't))
(check-equal? '(s w x) (fewest-vertices-path g22.3b 's 'x))
(check-equal? '(s w x u) (fewest-vertices-path g22.3b 's 'u))
(check-equal? '(s w x y) (fewest-vertices-path g22.3b 's 'y))

;; dfs utils ------------------------------------------------------------------

(define (verify-π d π)
  (define sorted (sort (hash-keys d) < #:key (λ (k) (hash-ref d k))))
  (check-false (hash-ref π (car sorted)))
  (let loop ([sorted sorted])
    (unless (null? (cdr sorted))
      (when (= (add1 (hash-ref d (car sorted))) (hash-ref d (cadr sorted)))
        (check-equal? (hash-ref π (cadr sorted)) (car sorted)))
      (loop (cdr sorted)))))

;; if there are x vertices, then the values in d and f should include all the
;; numbers from 1 to 2x
(define (verify-times d f)
  (check-equal? (+ (apply + (hash-values d)) (apply + (hash-values f)))
                (for/sum ([i (* 2 (length (hash-keys d)))]) i)))


;; dfs tests ------------------------------------------------------------------

(define g22.4 (mk-unweighted-graph/directed
               '((u x) (u v) (x v) (v y) (y x) (w y) (w z) (z z))))

(define-values (d22.4 π22.4 f22.4) (dfs g22.4))

;(check-true (all-black? color22.4))

;; started at 'v
;(check-equal? d22.4
;              (make-hash '((v . 1) (y . 2) (x . 3) (w . 7) (z . 8) (u . 11))))
;(check-equal? π22.4
;              (make-hash '((v . #f) (y . v) (x . y) (w . #f) (z . w) (u . #f))))
;(check-equal? f22.4
;              (make-hash '((x . 4) (y . 5) (v . 6) (z . 9) (w . 10) (u . 12))))

(verify-π d22.4 π22.4)
(verify-times d22.4 f22.4)

(define g22.4b (mk-directed-graph
               '((u x) (u v) (x v) (v y) (y x) (w y) (w z) (z z))))

(define-values (d22.4b π22.4b f22.4b) (dfs g22.4b))

(verify-π d22.4b π22.4b)
(verify-times d22.4b f22.4b)


(define g22.5 
  (mk-unweighted-graph/directed
   '((y x) (z y) (x z) (z w) (w x) (s z) (s w) (v s) (v w) (t v) (t u) (u t) (u v))))

(define-values (d22.5 π22.5 f22.5) (dfs g22.5))
;(check-true (all-black? color22.5))
;(verify-π d22.5 π22.5)
;(verify-times d22.5 f22.5)

;;; started at 'w
;(check-equal? 
; d22.5
; (make-hash '((t . 10) (y . 4) (z . 3) (u . 9) (s . 12) (x . 2) (v . 11) (w . 1))))
;(check-equal? 
; π22.5
; (make-hash '((t . u) (y . z) (z . x) (u . #f) (s . v) (v . t) (x . w) (w . #f))))
;(check-equal? 
; f22.5
; (make-hash '((x . 7) (y . 5) (z . 6) (v . 14) (s . 13) (t . 15) (u . 16) (w . 8))))

(verify-π d22.5 π22.5)
(verify-times d22.5 f22.5)


(define g22.5b
  (mk-directed-graph
   '((y x) (z y) (x z) (z w) (w x) (s z) (s w) (v s) (v w) (t v) (t u) (u t) (u v))))

(define-values (d22.5b π22.5b f22.5b) (dfs g22.5b))
(verify-π d22.5b π22.5b)
(verify-times d22.5b f22.5b)

(check-false (or (dag? g22.3) (dag? g22.4) (dag? g22.5)))
(check-false (or (dag? g22.3b) (dag? g22.4b) (dag? g22.5b)))



(define g22.6 
  (mk-unweighted-graph/directed 
   '((s v) (v w) (w s) (q s) (q w) (q t) (t y) (t x) (x z) (z x) (y q) (r y) (u y) (r u))))

(check-false (dag? g22.6))

; 'r start
(define-values (d22.6 π22.6 f22.6) (dfs g22.6))

;(check-true (all-black? color22.6))
(verify-π d22.6 π22.6)
(verify-times d22.6 f22.6)

;(check-equal?
; d22.6
; (make-hash 
;  '((t . 10) (y . 2) (z . 12) (v . 6) (s . 5) (x . 11) (q . 3) (u . 18) (w . 4) (r . 1))))
;
;(check-equal?
; π22.6
; (make-hash 
;  '((t . q) (y . r) (z . x) (u . r) (s . w) (v . s) (q . y) (x . t) (w . q) (r . #f))))
;
;(check-equal?
; f22.6
; (make-hash
;  '((t . 15) (y . 17) (z . 13) (v . 7) (s . 8) (x . 14) (q . 16) (u . 19) (w . 9) (r . 20))))

(define g22.6b 
  (mk-directed-graph
   '((s v) (v w) (w s) (q s) (q w) (q t) (t y) (t x) (x z) (z x) (y q) (r y) (u y) (r u))))

(check-false (dag? g22.6b))

; 'r start
(define-values (d22.6b π22.6b f22.6b) (dfs g22.6b))

(verify-π d22.6b π22.6b)
(verify-times d22.6b f22.6b)




;; tsort ----------------------------------------------------------------------
(define g22.7 
  (mk-unweighted-graph/directed
   '((undershorts pants) (undershorts shoes) (pants belt) (pants shoes) (belt jacket)
     (shirt belt) (shirt tie) (tie jacket) (socks shoes) watch)))

(do-tsort-tests g22.7)

(define g22.8 (mk-unweighted-graph/adj '((m r q x)
                                         (n q u o)
                                         (o r v s)
                                         (p o s z)
                                         (q t)
                                         (r u y)
                                         (s r)
                                         (t)
                                         (u t)
                                         (v x w)
                                         (w z)
                                         (x)
                                         (y v)
                                         (z))))

(do-tsort-tests g22.8)

(define g22.7b 
  (mk-directed-graph
   '((undershorts pants) (undershorts shoes) (pants belt) (pants shoes) (belt jacket)
     (shirt belt) (shirt tie) (tie jacket) (socks shoes) watch)))

(do-tsort-tests g22.7b)


;; strongly connected components (scc) ----------------------------------------

(define g22.9-edges 
  '((a b) (b f) (b e) (b c) (e a) (e f) (f g)
    (g f) (c d) (d c) (c g) (d h) (g h) (h h)))
(define g22.9 (mk-unweighted-graph/directed g22.9-edges))
(define g22.9^T (mk-unweighted-graph/directed 
                 (for/list ([e g22.9-edges]) (reverse e))))

(check-equal? g22.9^T (transpose g22.9))

(define g22.9-adj (mk-unweighted-graph/adj '((a b)
                                             (b e f c)
                                             (c d g)
                                             (d c h)
                                             (e a f)
                                             (f g)
                                             (g f h)
                                             (h h))))

(check-equal? g22.9 g22.9-adj)

(define scc22.9 (scc g22.9))
(check-equal? (set (set 'a 'b 'e) (set 'f 'g) (set 'c 'd) (set 'h))
              (apply set (map (λ (vs) (apply set vs)) scc22.9)))

(define g22.9b (mk-directed-graph g22.9-edges))
(define g22.9b^T (mk-directed-graph (for/list ([e g22.9-edges]) (reverse e))))

(check-equal? g22.9b^T (transpose g22.9b))

(define scc22.9b (scc g22.9b))
(check-equal? (set (set 'a 'b 'e) (set 'f 'g) (set 'c 'd) (set 'h))
              (apply set (map (λ (vs) (apply set vs)) scc22.9b)))

;; ----------------------------------------------------------------------------
;; test remove-vertex!

;; unweighted graphs:
(define g/vertex-removed
  (mk-unweighted-graph/adj '((a b c) (b c a) (c b a) (d c b))))
(remove-vertex! g/vertex-removed 'a)
(check-seqs-as-equal-sets? (in-vertices g/vertex-removed)
                           '(b c d))
(check-seqs-as-equal-sets? (in-edges g/vertex-removed)
                           '((b c) (c b) (d c) (d b)))
;; remove non-existent vertex
(remove-vertex! g/vertex-removed 'a)
(check-seqs-as-equal-sets? (in-vertices g/vertex-removed)
                           '(b c d))
(check-seqs-as-equal-sets? (in-edges g/vertex-removed)
                           '((b c) (c b) (d b) (d c)))

;; test vertex-remove!: weighted graphs
(define g/wgt/vertex-removed
  (mk-weighted-graph/directed '((1 a b) (2 a c) (3 b c) (4 b a)
                                (5 c b) (6 c a) (7 d c) (8 d b))))
(define edges/old (get-edges g/wgt/vertex-removed))
(remove-vertex! g/wgt/vertex-removed 'a)
(check-seqs-as-equal-sets? (in-vertices g/wgt/vertex-removed)
                           '(b c d))
(check-seqs-as-equal-sets? (in-edges g/wgt/vertex-removed)
                           '((b c) (c b) (d c) (d b)))

(define g/wgt/vr2 
  (mk-directed-graph 
   '((a b) (a c) (b c) (b a) (c b) (c a) (d c)  (d b))
   '(1 2 3 4 5 6 7 8)))
(remove-vertex! g/wgt/vr2 'a)

(check-seqs-as-equal-sets? (in-vertices g/wgt/vr2)
                           '(b c d))
(check-seqs-as-equal-sets? (in-edges g/wgt/vr2)
                           '((b c) (c b) (d c) (d b)))

;; non-existent edges have weight infty
(for ([e edges/old] #:when (member 'a e))
  (check-equal? (apply edge-weight g/wgt/vertex-removed e) +inf.0))

;; remove non-existent vertex
(remove-vertex! g/wgt/vertex-removed 'a)
(check-seqs-as-equal-sets? (in-vertices g/wgt/vertex-removed)
                           '(b c d))
(check-seqs-as-equal-sets? (in-edges g/wgt/vertex-removed)
                           '((b c) (c b) (d b) (d c)))

;; ----------------------------------------------------------------------------
;; test rename-vertex!

(define g/to-rename
  (mk-unweighted-graph/adj '((a b c) (b c a) (c b a) (d c b))))
(rename-vertex! g/to-rename 'a 'e)
(check-seqs-as-equal-sets? (in-vertices g/to-rename) '(e b c d))
(check-seqs-as-equal-sets? (in-edges g/to-rename)
                           '((e b) (e c) (b c) (b e) (c b) (c e) (d c) (d b)))

(rename-vertex! g/to-rename 'e 'a)
(check-equal? g/to-rename
              (mk-unweighted-graph/adj '((a b c) (b c a) (c b a) (d c b))))
(check-seqs-as-equal-sets? (in-vertices g/to-rename) '(a b c d))
(check-seqs-as-equal-sets? (in-edges g/to-rename)
                           '((a b) (a c) (b c) (b a) (c b) (c a) (d c) (d b)))
              
;; rename to same name -- errors
(check-exn exn:fail? (λ () (rename-vertex! g/to-rename 'a 'a)))
;; rename to existing vertex -- error
(check-exn exn:fail? (λ () (rename-vertex! g/to-rename 'a 'b)))

;; check #f fewest-vertices-path
(define g/no-path (mk-directed-graph '((a b) (c d))))
(check-false (fewest-vertices-path g/no-path 'a 'd))

;; ----------------------------------------------------------------------------
;; test different do-bfs and do-dfs options
;; Thanks to Jason Hemann for discovering some of the problems

;; re-define dag?, but explicitly name the vertices in each clause
(define (dag?-explicit-binds G)
  (define-vertex-property G color #:init WHITE)
  (define not-dag? #f)
  (do-dfs G 
   #:break (from to) not-dag?
   #:visit? (from to) (white? (color to))
   #:prologue (from to) (color-set! to GRAY)
   #:epilogue (from to) (color-set! to BLACK)
   #:process-unvisited? (from to) (gray? (color to))
   #:process-unvisited (from to) (set! not-dag? #t)
   #:return (acc) (not not-dag?)))
(check-true (dag?-explicit-binds g-adj1))
(check-false (or (dag?-explicit-binds g22.3) 
                 (dag?-explicit-binds g22.4)
                 (dag?-explicit-binds g22.5)))
(check-false (or (dag?-explicit-binds g22.3b)
                 (dag?-explicit-binds g22.4b)
                 (dag?-explicit-binds g22.5b)))
(check-false (dag?-explicit-binds g22.6))
(check-false (dag?-explicit-binds g22.6b))

;; re-define dag?, but use an accum instead of a global flag
(define (dag?/acc G)
  (define-vertex-property G color #:init WHITE)
  (do-dfs G 
   #:init #f ; accum represents not-dag?
   #:break (from to not-dag?) not-dag?
   #:visit? (from to) (white? (color to))
   #:prologue (from to) (color-set! to GRAY)
   #:epilogue (from to) (color-set! to BLACK)
   #:process-unvisited? (from to) (gray? (color to))
   #:process-unvisited (from to not-dag?) #t ; found a cycle, so set accum
   #:return (not-dag?) (not not-dag?)))
(check-true (dag?/acc g-adj1))
(check-false (or (dag?/acc g22.3) 
                 (dag?/acc g22.4)
                 (dag?/acc g22.5)))
(check-false (or (dag?/acc g22.3b)
                 (dag?/acc g22.4b)
                 (dag?/acc g22.5b)))
(check-false (dag?/acc g22.6))
(check-false (dag?/acc g22.6b))
