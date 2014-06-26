#lang racket

(require (for-syntax racket/syntax syntax/parse 
                     syntax/parse/experimental/template)
         "gen-graph.rkt")

(provide (all-defined-out))

(define-syntax (define-vertex-property stx)
  (syntax-parse stx
    [(_ g prop (~optional (~seq #:init init-val)))
     (with-syntax 
       ([hash-name (generate-temporary #'prop)]
        [hash-name-set! (format-id #'prop "~a-set!" #'prop)])
       (template
        (begin
          (define hash-name (make-hash))
          (define-syntax prop
            (syntax-id-rules (set!)
              [(set! prop new-h) (set! hash-name new-h)]
              [(_ key) (hash-ref hash-name key)]
              [(_ key fail) (hash-ref hash-name key (thunk fail))]
              [prop hash-name]))
          (define (hash-name-set! key val) (hash-set! hash-name key val))
          (?? (for ([v (in-vertices g)]) (hash-name-set! v init-val))))))]))

(define-syntax-rule (define-vertex-properties g p ...) 
  (begin (define-vertex-property g p) ...))

(define-syntax (define-graph-property stx)
  (syntax-parse stx
    [(_ prop v) 
     #:with prop-set! (format-id #'prop "~a-set!" #'prop)
     #:with prop-get (format-id #'prop "get-~a" #'prop)
     (template
      (begin
        (define prop v)
        (define (prop-set! x) (set! prop x))
        (define (prop-get) prop)))]))
                      
