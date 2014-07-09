#lang racket

(require rackunit racket/unsafe/ops file/gunzip)

(require "../graph-weighted.rkt"
         "../graph-fns-basic.rkt"
         "../gen-graph.rkt"
         "test-utils.rkt")

;; times determined from running cmd line racket, on 2.40Ghz quad-core cpu (Q6600)
;; otherwise when indicated, times recorded using i7-2600k

;; unzip file
(when (not (file-exists? "SCC.txt")) (gunzip "SCC.txt.gz"))

;; scc timing test ------------------------------------------------------------
;; large (875714 vertices) directed graph (unweighted) to check timing
;; from Coursera Stanford Algorithms course
;; see also https://class.coursera.org/algo-005/forum/thread?thread_id=623

(define g/scc (mk-directed-graph null))

;; ~1sec to just read the file
;; ~20sec to (map string->number (string-split e)) every line
;; about ~40sec to create the graph
;; no difference when using with-input-from-file and open-input-file
;; using in-port: ~28sec
;; unsafe-struct-ref: ~28sec
;; on work machine: ~22s
;; custom read-num (work machine): ~12.5s
;; read-num with unsafe fx ops: ~12.5s
(with-input-from-file "SCC.txt"
  (λ ()
    ;; SCC.txt file has format: num <space> num <space> \n
    ;; (last line has no \n)
    (define (read-num [in (current-input-port)])
      (let loop ([num 0])
        (define b (read-byte in))
        (if (eof-object? b) eof
            (case b
              [(10) (loop 0)] ; #\newline
              [(32) num] ; #\space
              [else (loop (+ (* 10 num) (- b 48)))]))))
    (for ([u (in-port read-num)] [v (in-port read-num)]) 
      (add-directed-edge! g/scc u v))))
    ;(for ([u (in-port)]) (add-directed-edge! g/scc u (read)))))
;    (for ([e (in-lines)])
;      (apply add-directed-edge! g/scc (map string->number (string-split e))))))

;(displayln "graph created")

(check-equal? (length (get-vertices g/scc)) 875714)

;; old get-edges (using generator): ~35sec
;; dont use generator: ~3sec
;(time (length (get-edges g/scc)))
(check-equal? (length (get-edges g/scc)) 5105043)


;; TIMING NOTES (wikipedia scc fn --- dont use do-dfs)
;; use in-S? instead of member: from stuck to ~10s
;; use = instead of vertex=?: ~10s
;; unsafe list ops: ~<10s
;; use < instead of min: ~<10s (less than previous)
;; unsafe fx ops: ~9s
#;(define (fast-scc G)
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

;; TIMING NOTES (scc fn -- using do-dfs):
;; regular scc: stuck
;; regular scc with in-S? instead of member: ~12sec
;; = instead of vertex=?: ~12sec
;; syntax-rules: ~12sec
;; unsafe list ops: ~12sec
;; unsafe arith ops: ~12sec
;; < instead of min (saves a hash-set): ~11.5sec
;; unsafe <: ~11.3sec
;; use acc: ~11.3sec
;; cache llv in epilogue (saves a lookup: ~11.2sec
;; dont use broke? parameter: ~10.2sec
;; duplicate broken? flag: ~10.3sec
;; unsafe-struct-ref: ~10.3sec
;; use dont use generator for edges: ~9s

;; expected: 371762 sccs
(define SCC-TIME-LIMIT 10000) ; see timing notes above
(let-values ([(res cpu real gc) (time-apply scc (list g/scc unsafe-fx=))])
  (check-equal? 371762 (length (car res)))
  (when (> real SCC-TIME-LIMIT) 
    (printf "scc time: ~a sec (LIMIT = ~a)\n" real SCC-TIME-LIMIT))
  (check-true (< real SCC-TIME-LIMIT)))

