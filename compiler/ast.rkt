#lang racket/base

(require racket/match
         "types.rkt")

(struct binding (type name value) #:transparent)

(struct tl-definition (name value) #:transparent)
(struct tl-annotate (name type) #:transparent)
(struct tl-struct (name fields) #:transparent )

(struct expr-lit (value) #:transparent) 
(struct expr-var (name) #:transparent)
(struct expr-if (cond then else) #:transparent)
(struct expr-let (bindings body) #:transparent)
(struct expr-begin (exprs) #:transparent)
(struct expr-lambda (params body) #:transparent)
(struct expr-call (func args) #:transparent)

(define (literal? v)
  (or (and (number? v) (exact? v))
      (char? v)
      (boolean? v)))

(define (parse-top form)
  (match form
    [`(: ,n ,t) (tl-annotate n (parse-type t))]
    [`(define (,n ,ps ...) ,body ...)
     (tl-definition n (expr-lambda
                       ps (expr-begin (map parse-expr body))))]
    [`(define ,n ,val) (tl-definition n (parse-expr val))]
    [_ (error (format "error: not a top level form: ~a" form))]))

(define (parse-expr expr)
  (match expr
    [v #:when (literal? v) (expr-lit v)]
    [v #:when (symbol? v) (expr-var v)]
    [`(if ,c ,t ,e) (expr-if (parse-expr c)
                             (parse-expr t)
                             (parse-expr e))]
    [`(let ((,ts ,ns ,vs) ...) ,body)
     (expr-let
      (map
       (λ (t n v) (binding (parse-type t)
                           n
                           (parse-expr v)))
       ts ns vs)
      (expr-begin (map parse-expr body)))]
    [`(begin ,exprs ...) (expr-begin (map parse-expr exprs))]
    [`(,fn ,xs ...) (expr-call (parse-expr fn)
                               (map parse-expr xs))]
    [_ (error (format "error: not a valid epxression: ~a" expr))]))


(define (parse-program forms)
  (map parse-top forms))

(parse-program
 '((: add (-> int int int))
   (define (add x y) (+ x y))
   (: main (-> unit unit))
   (define (main)
     (add 1 2))))


(parse-program
 '((: foo int)
   (define foo 42)
   (: add (-> int int int))
   (define (add x y) (let ((int z 1)
                           (int w 3))
                       (+ x y z w)))))



(provide (all-defined-out))
