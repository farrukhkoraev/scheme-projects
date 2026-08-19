#lang racket/base

(require racket/match
         racket/string)


;; int char bool unit

(struct function-t (params return) #:transparent)
(struct pointer-t (base) #:transparent)
(struct struct-t (name) #:transparent)


(define (primitive-type? t)
  (if (member t '(int char bool unit))
      #t
      #f))


(define (type->string t)
  (match t
    [t #:when (primitive-type? t) (symbol->string t)]
    [(pointer-t t) (format "*~a" (type->string t))]
    [(function-t ps ret)
     (format "-> ~s ~s"
             (string-join (map type->string ps))
             (type->string ret))]))

(define (parse-type sexp)
  (match sexp
    [t  #:when (primitive-type? t) t]
    [`(ptr ,t) #:when (not (eq? t 'unit))
               (pointer-t (parse-type t))]
    [`(-> ,params ... ,ret)
     (function-t (map parse-type params) (parse-type ret))]
    [t (error (format "unknown type: ~a" t))]))

(define (type-equal? t1 t2)
  (equal? t1 t2))


(provide parse-type
         type->string
         type-equal?
         function-t
         function-t-params
         function-t-return)
