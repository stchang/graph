#lang racket

(require "hash-utils.rkt"
         "utils.rkt"
         "gen-graph.rkt"
         (only-in "../queue/fifo.rkt" mk-empty-fifo)
         "../queue/gen-queue.rkt")

(require racket/generator)

(provide (except-out (all-defined-out)))

;; general fns ----------------------------------------------------------------

(define (vertex? g v) (and (member v (in-vertices g)) #t))

(define (edge? g u v)
  (and (vertex? g u) (vertex? g v)
       (member v (sequence->list (in-neighbors g u)))
       #t))

;; returns edges as a sequence
(define (in-edges g)
  (in-generator 
   (for* ([u (in-vertices g)] [v (in-neighbors g u)]) 
     (yield (list u v)))))


;; ----------------------------------------------------------------------------
;; bfs and dfs

(define-values (bfs-init
                bfs-process-neighbor?
                bfs-process-neighbor
                bfs-post-visit
                bfs-finish)
  (let ()
    (define-hashes color d π)
                
    (define (bfs-init G s)
      (define-hashes new-color new-d new-π)
      (set! color new-color)
      (set! d new-d)
      (set! π new-π)
  
      (for ([u (in-vertices G)])
        (cond [(equal? s u) (color-set! s GRAY)
                            (d-set!     s 0)
                            (π-set!     s #f)]
              [else         (color-set! u WHITE)
                            (d-set!     u +inf.0)
                            (π-set!     u #f)])))

    (define (bfs-process-neighbor? v) (white? (color v)))
  
    (define (bfs-process-neighbor u v)
      (color-set! v GRAY)
      (d-set!     v (add1 (d u)))
      (π-set!     v u))
    
    (define (bfs-post-visit u) (color-set! u BLACK))
    
    (define (bfs-finish) (values color d π))
    
    (values bfs-init 
            bfs-process-neighbor?
            bfs-process-neighbor
            bfs-post-visit
            bfs-finish)))

;; bfs : Graph Vertex -> 
;; s is the source vertex
(define (bfs G s 
             #:make-queue [make-queue mk-empty-fifo]
             #:init [init bfs-init]
             #:process-neighbor? [process-neighbor? bfs-process-neighbor?]
             #:process-neighbor [process-neighbor bfs-process-neighbor]
             #:post-visit [post-visit bfs-post-visit]
             #:finish [finish bfs-finish])
;  (define-hashes color d π)
;  
;  (for ([u (in-vertices G)])
;    (cond [(equal? s u) (color-set! s GRAY)
;                        (d-set!     s 0)
;                        (π-set!     s #f)]
;          [else         (color-set! u WHITE)
;                        (d-set!     u +inf.0)
;                        (π-set!     u #f)]))
  (init G s)
  
  (define Q (make-queue))
  (enqueue! Q s)
  (let loop () (unless (empty? Q)
    (define u (dequeue! Q))
    (for ([v (in-neighbors G u)] #:when (process-neighbor? v))
      (process-neighbor u v)
;      (color-set! v GRAY)
;      (d-set!     v (add1 (d u)))
;      (π-set!     v u)
      (enqueue! Q v))
    (post-visit u)
;    (color-set! u BLACK)
    (loop)))
  (finish))
;  (values color d π))

;; returns shortest path in G from source s to v
(define (shortest-path G s v)
  (define-values (color d π) (bfs G s))
  (reverse
   (let loop ([v v])
     (if (equal? v s) (list s)
         (let ([πv (hash-ref π v
                     (λ() (error 'shortest-path "no vertex ~a in graph ~a" v G)))])
           (if πv (cons v (loop πv))
               (error 'shortest-path "no path from ~a to ~a in graph ~a" s v G)))))))


(define (dfs G #:order [order (λ (vs) vs)])
  ;; d[u] = discovery time, f[u] = finishing time
  (define-hashes color d π f)
  (for ([u (in-vertices G)]) 
    (color-set! u WHITE)
    (π-set!     u #f))
  (define time 0)
  
  (for ([u (order (in-vertices G))] #:when (white? (color u)))
    (let dfs-visit ([u u])
      (color-set! u GRAY)
      (add1! time)
      (d-set! u time)
      (for ([v (in-neighbors G u)] #:when (white? (color v)))
        (π-set! v u)
        (dfs-visit v))
      (color-set! u BLACK)
      (add1! time)
      (f-set! u time)))
  
  (values color d π f))

(define (dag? G)
  (define-hashes color)
  (for ([u (in-vertices G)]) (color-set! u WHITE))
  
  (for/and ([u (in-vertices G)] #:when (white? (color u)))
    (let dfs-visit ([u u])
      (color-set! u GRAY)
      (begin0
        (for/and ([v (in-neighbors G u)])
          (and (not (gray? (color v)))
               (or (black? (color v))
                   (dfs-visit v))))
        (color-set! u BLACK)))))

(define (tsort G)
  (define-values (color d π f) (dfs G))
  (sort (hash-keys f) > #:key (λ (k) (hash-ref f k))))

  
;; tarjan algorithm for strongly connected components
(define (scc G)
  (define i 0)
  (define-hashes index lowlink)
  (define S null)
  (define (S-push x) (set! S (cons x S)))
  
  (define SCC null)
  
  (define (strongconnect v)
    (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v)
    
    (for ([w (in-neighbors G v)])
      (cond [(not (hash-has-key? index w))
             (strongconnect w)
             (lowlink-set! v (min (lowlink v) (lowlink w)))]
            [(member w S)
             (lowlink-set! v (min (lowlink v) (index w)))]))
    
    (when (= (lowlink v) (index v)) ;; v is root node, so create an SCC
      (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (equal? w v)))))
      (set! SCC (cons (cons v new-scc) SCC))
      (set! S (cdr S-rst))))
  
  (for ([v (in-vertices G)] #:unless (hash-has-key? index v)) (strongconnect v))
  
  SCC)