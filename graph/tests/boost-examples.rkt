#lang racket
(require "../main.rkt"
         "test-utils.rkt"
         "../graph-property.rkt"
         "../utils.rkt")
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

;; more examples from http://www.boost.org/doc/libs/1_55_0/libs/graph/example/

;; bfs: http://www.boost.org/doc/libs/1_55_0/libs/graph/example/bfs-example2.cpp
(define g/bfs2 (undirected-graph 
                '((r s) (r v) (s w) (w r) (w t) (w x) (x t) (t u) (x y) (u y))))
(define-values (d/bfs2 π/bfs2) (bfs g/bfs2 's))

;; boost output: '(s r w v t x u y)
(let ([res (sort (hash-keys d/bfs2) < #:key (λ (v) (hash-ref d/bfs2 v)))])
  (define res/sets 
    (match res
      [`(,a ,b ,c ,d ,e ,f ,g ,h)
       `(,(set a)
         ,(set b c)
         ,(set d e f)
         ,(set g h))]))
  (check-equal? res/sets (list (set 's)
                               (set 'r 'w)
                               (set 'v 't 'x)
                               (set 'u 'y))))

;; another bfs: http://www.boost.org/doc/libs/1_55_0/libs/graph/example/bfs-name-printer.cpp
(define g/bfs-name
  (undirected-graph '((a b) (a d) (b d) (c a) (c e) (d c) (d e))))
(let-values ([(d π) (bfs g/bfs-name 'a)])
  (define res (sort (hash-keys d) < #:key (λ (v) (hash-ref d v))))
  (define res/sets
    (match res
      [`(,a ,b ,c ,d ,e)
       `(,(set a)
         ,(set b c d)
         ,(set e))]))
  (check-equal? res/sets (list (set'a)
                               (set 'b 'c 'd)
                               (set 'e))))

;; dfs parens: http://www.boost.org/doc/libs/1_55_0/libs/graph/example/dfs_parenthesis.cpp

(define g/parens
  (undirected-graph '((0 2) (1 1) (1 3) (2 1) (2 3) (3 1) (3 4) (4 0) (4 1))))
(check-equal?
 (with-output-to-string
  (λ ()
    (do-dfs g/parens
     #:prologue: (printf "(~a" $v)
     #:epilogue: (printf "~a)" $v))))
 "(0(2(1(3(44)3)1)2)0)")

;; graph copying
(define g/for-copy
  (undirected-graph '((a c) (a d) (b a) (b d) (c f) (d c) (d e) (d f) (e b) (e g) (f e) (f g))))
(define g/copy (graph-copy g/for-copy))
(check-false (eq? g/for-copy g/copy))
(check-equal? g/for-copy g/copy)

; bipartite: http://www.boost.org/doc/libs/1_55_0/libs/graph/example/bipartite_example.cpp
(define g/bi 
  (undirected-graph 
   '((0 1) (1 2) (0 4) (2 6) (4 5) (5 6) (4 7) (6 7) (3 4) (3 8) (7 10) (8 9) (9 1))))
(let ([res (bipartite? g/bi)])
  (check-not-false res)
  (define white (set 0 2 3 5 7 9))
  (define black (set 1 4 6 8 10))
  (check-true (or (and (equal? white (apply set (first res)))
                       (equal? black (apply set (second res))))
                  (and (equal? black (apply set (first res)))
                       (equal? white (apply set (second res)))))))

(define g/non-bi
  (undirected-graph
   '((0 1) (0 4) (1 2) (2 6) (3 6) (3 8) (4 5) (4 7) (5 6) (6 7) (7 9) (8 9))))
(check-false (bipartite? g/non-bi))

;; connected components
;; cc-internet: http://www.boost.org/doc/libs/1_55_0/libs/graph/example/cc-internet.cpp
(define g/net
  (undirected-graph
   '(("engr-fe21.gw.nd.edu" "shub-e27.gw.nd.edu")
     ("shub-e27.gw.nd.edu" "chicago1-nbr1.bbnplanet.net")
     ("shub-e27.gw.nd.edu" "core1-ord1-oc48.ord2.above.net")
     ("chicago1-nbr1.bbnplanet.net" "above-bbn-45Mbps.ord.above.net")
     ("above-bbn-45Mbps.ord.above.net" "engr-fe21.gw.nd.edu")
     ("above-bbn-45Mbps.ord.above.net" "core1-ord1-oc48.ord2.above.net")
     ("core1-ord1-oc48.ord2.above.net" "vabi1-gige-1-1.google.com")
     ("vabi1-gige-1-1.google.com" "chicago1-nbr1.bbnplanet.net")
     
     ("cambridge1-nbr2.bbnplanet.net" "boston1-br1.bbnplanet.net")
     ("ihtfp.mit.edu" "boston1-br1.bbnplanet.net")
     ("cambridge1-nbr2.bbnplanet.net" "radole.lcs.mit.edu")
     ("ihtfp.mit.edu" "radole.lcs.mit.edu")
     
     ("helios.ee.lbl.gov" "lilac-dmc.Berkeley.EDU")
     ("lilac-dmc.Berkeley.EDU" "ccngw-ner-cc.Berkeley.EDU")
     ("ccngw-ner-cc.Berkeley.EDU" "ccn-nerif35.Berkeley.EDU")
     ("ccn-nerif35.Berkeley.EDU" "rip.Berkeley.EDU")
     ("helios.ee.lbl.gov" "ccn-nerif35.Berkeley.EDU")
     ("lilac-dmc.Berkeley.EDU" "rip.Berkeley.EDU")
     
     ("nycmny1-cr1.bbnplanet.net" "teledk.bbnplanet.net")
     ("teledk.bbnplanet.net" "albnxg1.ip.tele.dk")
     ("albnxg1.ip.tele.dk" "gw-dkuug.oeb.tdk.ne")
     ("nycmny1-cr1.bbnplanet.net" "gw-dkuug.oeb.tdk.ne")
     ("nycmny1-cr1.bbnplanet.net" "albnxg1.ip.tele.dk"))))

(let ([res (cc g/net)]
      [res/bfs (cc/bfs g/net)])
  (define expected
    '((vabi1-gige-1-1.google.com engr-fe21.gw.nd.edu above-bbn-45Mbps.ord.above.net core1-ord1-oc48.ord2.above.net shub-e27.gw.nd.edu chicago1-nbr1.bbnplanet.net)
      (albnxg1.ip.tele.dk teledk.bbnplanet.net nycmny1-cr1.bbnplanet.net gw-dkuug.oeb.tdk.ne)
      (helios.ee.lbl.gov rip.Berkeley.EDU lilac-dmc.Berkeley.EDU ccngw-ner-cc.Berkeley.EDU ccn-nerif35.Berkeley.EDU)
      (ihtfp.mit.edu boston1-br1.bbnplanet.net cambridge1-nbr2.bbnplanet.net radole.lcs.mit.edu)))
  (check-equal? (apply set (map (λ (cc) (apply set (map string->symbol cc))) res))
                (apply set (map (λ (cc) (apply set cc)) expected)))
  (check-equal? (apply set (map (λ (cc) (apply set cc)) expected))
                (apply set (map (λ (cc) (apply set (map string->symbol cc))) res/bfs))))

;; scc: http://www.boost.org/doc/libs/1_55_0/libs/graph/example/strong_components.cpp
(define g/scc (unweighted-graph/adj '((a b f h)
                                      (b c a)
                                      (c d b)
                                      (d e)
                                      (e d)
                                      (f g)
                                      (g f d)
                                      (h i)
                                      (i h j e c)
                                      (j))))

(define sccs (scc g/scc))
(check-equal? (length sccs) 4)
(check-equal? (apply set (map (λ (cc) (apply set cc)) sccs))
              (set (set 'a 'b 'c 'h 'i)
                   (set 'd 'e)
                   (set 'f 'g)
                   (set 'j)))
