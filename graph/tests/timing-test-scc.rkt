#lang racket

(require "../graph-weighted.rkt"
         "../graph-fns-basic.rkt"
         "../gen-graph.rkt"
         "../graph-property.rkt"
         "test-utils.rkt")

(require rackunit)

;; limits determined running cmd line racket, on 2.40Ghz quad-core

;; scc timing test ------------------------------------------------------------
;; large (875714 vertices) directed graph (unweighted) to check timing
;; from Coursera Stanford Algorithms course

(define g/scc (mk-directed-graph null))

;; ~1sec to just read the file
;; ~20sec to (map string->number (string-split e)) every line
;; about ~40sec to create the graph
;; no difference when using with-input-from-file and open-input-file
;; using in-port: ~28sec
(with-input-from-file "SCC.txt"
  (λ () (for ([u (in-port)])  (add-directed-edge! g/scc u (read)))))
;    (for ([e (in-lines)])
;      (apply add-directed-edge! g/scc (map string->number (string-split e))))))

(displayln "graph created")

(check-equal? (length (get-vertices g/scc)) 875714)

;(time (length (get-edges g/scc))) ; ~35sec
;(check-equal? (length (get-edges g/scc)) 5105043)

(require profile)

;; profiling just dfs takes ~111sec (~2min)
;(time (profile-thunk (λ () (begin (dfs g/scc) 1))))

;; profiling just the for loop and passthrough fns: ~54sec (~1min)
;; remove break? and process-unvisited? checks: ~47sec
;; dont profile: ~6sec
;; add broke? parameter back: ~8sec
;; remove combine and inner-init: ~8sec
;; remove acc: ~8sec
;; hash table instead of set for visited?: ~5sec
;; remove pro/epilogue: ~5sec
;; add scc pro/epilogue: ~7sec
;; add S: ~7sec
;; add build-scc?: ~7sec
;; add build-scc: ~8sec
;; add process-unvisited in inner: ~12sec
;; get-vertices in outer loop: ~12sec
;; move fast-scc to fn: ~10sec
;; (was getting wrong answer: #components = #verts ~800k)
;; reimplement directly from wikipedia article: stuck
;; reimplement from python code: stuck
;;   http://www.logarithmic.net/pfh-files/blog/01208083168/tarjan.py
#;(define (fast-scc G)
  (define init void)

  (define i 0)
  (define S null)   
  (define SCC null)
  (define (S-push x) (set! S (cons x S)))
  (define-vertex-properties G visited? index lowlink)
  (define (build-SCC? v) (= (lowlink v) (index v)))
  (define (build-SCC v)
    (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (vertex=? G w v)))))
    (set! SCC (cons (cons v new-scc) SCC))
    (set! S (cdr S-rst)))
  
  (define-syntax-rule (add1! x) (set! x (add1 x)))
  (define prologue (λ (G u v) (S-push v) (index-set! v i) (lowlink-set! v i) (add1! i) ))
  (define epilogue (λ (G from v) 
                     (when (build-SCC? v) (build-SCC v))
                     (when from (lowlink-set! from (min (lowlink from) (lowlink v))))))
  (define finish (λ _ SCC))
  
  (define (mark-visited! v) (visited?-set! v #t))
  (define (visit? G u v) (not (visited?-defined? v)))
  ;(define visited (set))
  ;(define (mark-visited! v) (set! visited (set-add visited v)))
  ;(define (visit? G u v) (not (set-member? visited v)))
  
  
  (define broke? (make-parameter #f))
  (broke? #f)
  (init G)
  ;; inner loop: keep following (unvisited) links
  (define (do-visit parent u)
    (mark-visited! u)
    (prologue G parent u)
    (for ([v (in-neighbors G u)] #:break (broke?))
      (cond [(visit? G u v) (do-visit u v)]
            [(member u S)
             (when parent (lowlink-set! parent (min (lowlink parent) (lowlink u))))]
            ))
    (epilogue G parent u))
  
  ;; outer loop: picks a new start node when previous search reaches dead end
  (for ([u (in-vertices G)] #:break (broke?))
    (cond [(visit? G #f u) (do-visit #f u)]
          #;[(member u S)
             (lowlink-set! #f (min (lowlink #f) (index u)))]
          ))
  
  (finish G))

#;(define (fast-scc G)
  (define i 0)
  (define-vertex-properties G visited? index lowlink)
  (define S null)
;  (define (S-push x) (set! S (cons x S)))
  (define SCC null)
;  (define (build-SCC? v) (= (lowlink v) (index v)))
;  (define (build-SCC v)
;    (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (vertex=? G w v)))))
;    (set! SCC (cons (cons v new-scc) SCC))
;    (set! S (cdr S-rst)))
  (define (strong-connect v)
    (index-set! v i)
    (lowlink-set! v i)
    (set! i (add1 i))
    (set! S (cons v S)) ; S.push
    
    (for ([w (in-neighbors G v)])
      (cond [(not (index-defined? w))
             (strong-connect w)
             (lowlink-set! v (min (lowlink v) (lowlink w)))]
            [(member w S)
             (lowlink-set! v (min (lowlink v) (lowlink w)))]))
    
    (when (= (lowlink v) (index v)) ; build SCC
      (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (vertex=? G w v)))))
      (set! SCC (cons (cons v new-scc) SCC))
      (set! S (cdr S-rst))))
      
  (for ([v (in-vertices G)]) (when (not (index-defined? v)) (strong-connect v)))
  SCC)

(require racket/unsafe/ops)
;; use in-S? instead of member: from stuck to ~10s
;; use = instead of vertex=?: ~10s
;; unsafe list ops: ~<10s
;; use < instead of min: ~<10s (less than previous)
;; unsafe fx ops: ~9s
(define (fast-scc G) ; python
  (define i 0)
  (define S null)
  (define SCC null)
  (define-vertex-properties G visited? lowlink index in-S?)

  (define (strong-connect v)
    (index-set! v i)
    (lowlink-set! v i)
    (set! i (unsafe-fx+ i 1))
    (set! S (unsafe-cons-list v S))
    (in-S?-set! v #t)

    (for ([w (in-neighbors G v)])
      (cond [(not (lowlink-defined? w))
             (strong-connect w)
             (let ([llv (lowlink v)][llw (lowlink w)]) (when (unsafe-fx< llw llv) (lowlink-set! v llw)))]
            [(in-S? w #:default #f) ;(member w S)
             (let ([llv (lowlink v)] [iw (index w)]) (when (unsafe-fx< iw llv) (lowlink-set! v iw)))
             #;(lowlink-set! v (min (lowlink v) (index w)))]))
    
    (when (= (lowlink v) (index v)) ; build SCC
      (define-values (new-scc S-rst) (splitf-at S (λ (w) (in-S?-set! w #f) (not (unsafe-fx= w v)))))
      (set! SCC (unsafe-cons-list (unsafe-cons-list v new-scc) SCC))
      (set! S (unsafe-cdr S-rst))))
  
  (for ([v (in-vertices G)]) (when (not (lowlink-defined? v)) (strong-connect v)))
  SCC)

;; 371762
(check-equal? 371762 (length (time (fast-scc g/scc))))

