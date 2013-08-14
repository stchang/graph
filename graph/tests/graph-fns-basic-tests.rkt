#lang racket
(require "../graph-unweighted.rkt" 
         "../gen-graph.rkt" 
         "../graph-fns-basic.rkt"
         "../utils.rkt"
         "test-utils.rkt")
;(require (rename-in graph 
;                    [unweighted-graph/undirected mk-unweighted-graph/undirected]
;                    [unweighted-graph/directed mk-unweighted-graph/directed]
;                    [unweighted-graph/adj mk-unweighted-graph/adj]))

(require rackunit)

;; bfs tests ------------------------------------------------------------------

;; graph from clrs figure 22.3
(define g22.3
  (mk-unweighted-graph/undirected
   '((r v) (r s) (s w) (w t) (w x) (x t) (t u) (x u) (x y) (y u))))

(check-equal? (apply set '(v r s w t x u y)) 
              (apply set (sequence->list (in-vertices g22.3))))

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
                (for/sum ([i (add1 (* 2 (length (hash-keys d))))]) i)))


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



(check-false (or (dag? g22.3) (dag? g22.4) (dag? g22.5)))



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

