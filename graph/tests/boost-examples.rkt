#lang racket
(require "../main.rkt"
         "test-utils.rkt"
         "../graph-property.rkt")
(require rackunit)

;; examples from boost.org

;; file dependencies example --------------------------------------------------

(define file-deps 
  (unweighted-graph/adj
   '((dax.h foo.cpp bar.cpp yow.h)
     (yow.h bar.cpp zag.cpp)
     (boz.h bar.cpp zig.cpp zag.cpp)
     (zow.h foo.cpp)
     (foo.cpp foo.o)
     (foo.o libfoobar.a)
     (bar.cpp bar.o)
     (bar.o libfoobar.a)
     (libfoobar.a libzigzag.a)
     (zig.cpp zig.o)
     (zig.o libzigzag.a)
     (zag.cpp zag.o)
     (zag.o libzigzag.a)
     (libzigzag.a killerapp)
     (killerapp))))

(do-tsort-tests file-deps)

;; for now, use transpose + in-neighbors to find all parents
;; TODO: add in-parents to gen:graph ?
(define (parallel-ordering g)
  (define g^T (transpose g))
  (define-vertex-property g time
    #:init (add1 (apply max -1 (for/list ([u (in-neighbors g^T $v)]) (time u))))
    #:vs (tsort g))
  (time->hash))

(check-equal? (parallel-ordering file-deps)
              (make-hash
               '((dax.h . 0)
                 (boz.h . 0)
                 (zow.h . 0)
                 (yow.h .   1)
                 (foo.cpp . 1)
                 (zig.cpp . 1)
                 (foo.o .   2)
                 (bar.cpp . 2)
                 (zig.o .   2)
                 (zag.cpp . 2)
                 (bar.o . 3)
                 (zag.o . 3)
                 (libfoobar.a . 4)
                 (libzigzag.a . 5)
                 (killerapp . 6))))

(define file-deps-with-cycle (graph-copy file-deps))
(add-edge! file-deps-with-cycle 'bar.cpp 'dax.h)
(check-false (dag? file-deps-with-cycle))
(do-tsort-tests file-deps) ;  make sure graph-copy makes an actual copy

;; six degrees of kevin bacon
(define actors
  (unweighted-graph/undirected
   (map (λ (s) 
          (define ss (string-split s ";")) 
          (list (first ss) (third ss)))
        (with-input-from-file "kevin-bacon.dat" port->lines))))
(define-vertex-property actors bacon-number
  #:init (sub1 (length (fewest-vertices-path actors $v "Kevin Bacon"))))

(define bacon-number-expected 
  (make-hash
   (map (λ (s) 
          (define ss (string-split s " has a Bacon number of "))
          (cons (first ss) (string->number (second ss))))
        (with-input-from-file "kevin-bacon-expected.dat" port->lines))))

(check-equal? (bacon-number->hash) bacon-number-expected)