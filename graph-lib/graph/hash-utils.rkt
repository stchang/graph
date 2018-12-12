#lang racket

(require (for-syntax racket/syntax)) ; format-id

(provide define-hash define-hashes)

(define-syntax (define-hash stx)
  (syntax-case stx ()
    [(_ hash-name)
     (with-syntax 
         ([(hash-name0) (generate-temporaries #'(hash-name))]
          [hash-name-set! (format-id #'hash-name "~a-set!" #'hash-name)])
       #'(begin
           (define hash-name0 (make-hash))
           (define-syntax hash-name
             (syntax-id-rules (set!)
               [(set! hash-name new-h) (set! hash-name0 new-h)]
               [(_ key) (hash-ref hash-name0 key)]
               [(_ key fail) (hash-ref hash-name0 key (thunk fail))]
               [hash-name hash-name0]))
           (define (hash-name-set! key value) (hash-set! hash-name0 key value))))]))
                      
(define-syntax-rule (define-hashes h ...) (begin (define-hash h) ...))