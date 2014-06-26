#lang racket

(require (for-syntax racket/syntax syntax/parse 
                     syntax/parse/experimental/template)
         "gen-graph.rkt"
         racket/stxparam)

(provide (all-defined-out))

;; TODO: make vertex and edge properties higher order?

(define-syntax-parameter $v (syntax-rules ()))

(define-syntax (define-vertex-property stx)
  (syntax-parse stx
    [(_ g prop (~or (~optional (~seq #:init init-expr:expr))
                    (~optional (~seq #:vs vs))) ...)
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
              [(_ key #:default fail) (hash-ref hash-name key (thunk fail))]
              [prop hash-name]))
          (define (hash-name-set! key val) (hash-set! hash-name key val))
          (?? (for ([v (?? vs (in-vertices g))])
                (hash-name-set! v 
                  (syntax-parameterize ([$v (syntax-id-rules () [_ v])])
                    init-expr)))))))]))

(define-syntax-parameter $from (syntax-rules ()))
(define-syntax-parameter $to (syntax-rules ()))

(define-syntax (define-edge-property stx)
  (syntax-parse stx
    [(_ g prop (~or (~optional (~seq #:init init-val:expr))
                    (~optional (~seq #:for-each init-expr:expr ...))) ...)
     (with-syntax 
       ([hash-name (generate-temporary #'prop)]
        [hash-name-set! (format-id #'prop "~a-set!" #'prop)])
       (template
        (begin
          (define hash-name (make-hash))
          (define-syntax prop
            (syntax-id-rules (set!)
              [(set! prop new-h) (set! hash-name new-h)]
              [(_ u v) (hash-ref hash-name (list u v))]
              [(_ u v #:default fail) (hash-ref hash-name (list u v) (thunk fail))]
              [prop hash-name]))
          (define (hash-name-set! u v val) (hash-set! hash-name (list u v) val))
          (?? (let ([vs (get-vertices g)])
                (for* ([i vs] [j vs])
                (hash-name-set! i j 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ i])]
                                        [$to (syntax-id-rules () [_ j])])
                    init-val)))))
          (?? (let ([vs (get-vertices g)])
                (for* ([i vs] [j vs])
                  (syntax-parameterize ([$from (syntax-id-rules () [_ i])]
                                        [$to (syntax-id-rules () [_ j])])
                    init-expr ...)))))))]))

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
                      
