#lang racket

(require rackunit racket/unsafe/ops file/gunzip)

(require "../graph-weighted.rkt"
         "../graph-fns-basic.rkt"
         "../gen-graph.rkt"
         "test-utils.rkt")

; test for computing traversal speed of in-neighbors

; times are in ms, run with racket 6.3 on the cmd line
; system specs
;$ lscpu
;Architecture:          x86_64
;CPU op-mode(s):        32-bit, 64-bit
;Byte Order:            Little Endian
;CPU(s):                8
;On-line CPU(s) list:   0-7
;Thread(s) per core:    2
;Core(s) per socket:    4
;Socket(s):             1
;NUMA node(s):          1
;Vendor ID:             GenuineIntel
;CPU family:            6
;Model:                 42
;Stepping:              7
;CPU MHz:               1600.000
;BogoMIPS:              6784.53
;Virtualization:        VT-x
;L1d cache:             32K
;L1i cache:             32K
;L2 cache:              256K
;L3 cache:              8192K
;NUMA node0 CPU(s):     0-7

(define VERBOSE #t)

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

(check-equal? (length (get-vertices g/scc)) 875714)

;; old get-edges (using generator): ~35sec
;; dont use generator: ~3sec
;(time (length (get-edges g/scc)))
(check-equal? (length (get-edges g/scc)) 5105043)

; just traverse vertices
(for ([i 10])
  (time
   (for* ([v (in-vertices g/scc)]
          [u (in-neighbors g/scc v)]
          [w (in-neighbors g/scc u)])
         (void))))
