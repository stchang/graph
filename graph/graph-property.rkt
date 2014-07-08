#lang racket/base

(require (for-syntax racket/base racket/syntax syntax/parse 
                     syntax/parse/experimental/template)
         "gen-graph.rkt" "utils.rkt"
         racket/stxparam)

(provide (all-defined-out))

;; TODO: make vertex and edge properties higher order?

(define-syntax (define-vertex-property stx)
  (syntax-parse stx
    [(_ g prop (~or (~optional (~seq #:init init-expr:expr))
                    (~optional (~seq #:vs vs))) ...)
     #:with hash-name (generate-temporary #'prop)
     #:with prop-set! (format-id #'prop "~a-set!" #'prop)
     #:with prop->hash (format-id #'prop "~a->hash" #'prop)
     #:with prop-defined? (format-id #'prop "~a-defined?" #'prop)
     #:with prop-count (format-id #'prop "~a-count" #'prop)
     (template
      (begin
        (define hash-name (make-hash))
        (define (prop key #:default 
                      [fail (λ () (error 'prop "no ~a value for ~a" 'prop key))])
          (hash-ref hash-name key fail))
        (define (prop->hash) hash-name)
        (define (prop-set! key val) (hash-set! hash-name key val))
        (define (prop-defined? key) (hash-has-key? hash-name key))
        (define (prop-count) (hash-count hash-name))
        (?? (for ([v (?? vs (in-vertices g))])
              (prop-set! v 
                (syntax-parameterize ([$v (syntax-id-rules () [_ v])])
                  init-expr))))))]))

(define-syntax (define-edge-property stx)
  (syntax-parse stx
    [(_ g prop (~or (~optional (~seq #:init init-val:expr))
                    (~optional (~seq #:for-each init-expr:expr ...))))
     #:with hash-name (generate-temporary #'prop)
     #:with prop-set! (format-id #'prop "~a-set!" #'prop)
     #:with prop->hash (format-id #'prop "~a->hash" #'prop)
     (template
      (begin
        (define hash-name (make-hash))
        (define (prop u v #:default 
                      [fail (λ () (error 'prop "no ~a value for edge ~a-~a" 
                                         'prop u v))])
          (hash-ref hash-name (list u v) fail))
        (define (prop->hash) hash-name)
        (define (prop-set! u v val) (hash-set! hash-name (list u v) val))
        (?? (let ([vs (get-vertices g)])
              (for* ([i vs] [j vs])
                (prop-set! i j 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ i])]
                                        [$to (syntax-id-rules () [_ j])])
                    init-val)))))
        (?? (let ([vs (get-vertices g)])
              (for* ([i vs] [j vs])
                (syntax-parameterize ([$from (syntax-id-rules () [_ i])]
                                      [$to (syntax-id-rules () [_ j])])
                  init-expr ...))))))]))

(define-syntax-rule (define-vertex-properties g p ...) 
  (begin (define-vertex-property g p) ...))

#;(define-syntax (define-graph-property stx)
  (syntax-parse stx
    [(_ prop v) 
     #:with prop-set! (format-id #'prop "~a-set!" #'prop)
     #:with prop-get (format-id #'prop "get-~a" #'prop)
     (template
      (begin
        (define prop v)
        (define (prop-set! x) (set! prop x))
        (define (prop-get . args) prop)))]))
                      
