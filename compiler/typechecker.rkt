#lang racket/base

(require racket/match
         racket/list
         "ast.rkt"
         "types.rkt")


(define fresh-env
  (hasheq '+  (function-t '(int int) 'int)
          '-  (function-t '(int int) 'int)
          '*  (function-t '(int int) 'int)
          '/  (function-t '(int int) 'int)
          '>  (function-t '(int int) 'bool)
          '>= (function-t '(int int) 'bool)
          '<  (function-t '(int int) 'bool)
          '<= (function-t '(int int) 'bool)
          '=  (function-t '(int int) 'bool)))

(define (extend-env env name val)
  (hash-set env name val)) 

(define (lookup-in env name)
  (let ([val (hash-ref env name #f)])
    (if val val
        (error (format "error: name ~a not found in environment" name)))))

(define (literal->type v)
  (cond
    [(exact? v) 'int]
    [(char? v) 'char]
    [(boolean? v) 'bool]))

(define (check-top form env)
  (match form
    [(tl-definition name (expr-lambda params body))
     (check-func-definition (lookup-in env name) params body env)]
    
    [(tl-definition name expr)
     (let ([t1 (lookup-in env name)]
           [t2 (check-expr expr env)])
       (if (type-equal? t1 t2) t1
           (error (format "error: type mismatch for ~a, expected ~a actual ~a"
                        name t1 t2))))]))

(define (check-func-definition type params body env)
  (define env*
    (foldl (λ (t n e) (extend-env e n t))
           env
           (function-t-params type)
           params))
  
  (let ([t (function-t-return type)]
        [t* (check-expr body env*)])
    (unless (type-equal? t t*)
      (error (format "type error: expected return type ~a, got ~a"
                     t t*)))
    type))


(define (check-expr expr env)
  (match expr
    [(expr-lit v) (literal->type v)]
    [(expr-var v) (lookup-in env v)]
    [(expr-if cnd thn els) (check-if-expr cnd thn els env)]
    [(expr-let bs body) (check-let-expr bs body env)]
    [(expr-begin exprs) (check-exprs exprs env)]
    [(expr-call fn args) (check-call-expr fn args env)]
    [(expr-lambda _ _) (error "lambda expressions are not allowed")]))

(define (check-if-expr cnd thn els env)
  (let ([t (check-expr cnd env)])
    (unless (equal? 'bool (check-expr cnd env))
      (error
       (format "type error: `if` condition clause expects bool got ~a"
               t)))
    (let ([t1 (check-expr thn env )]
          [t2 (check-expr els env)])
      (unless (equal? t1 t2)
        (error
         (format "type error: `if` expected to have ~a but has ~a"
                 t1 t2)))
      t1)))

(define (check-exprs exprs env)
  (let ([t (check-expr (first exprs) env)])
    (if (empty? (rest exprs))
        t
        (check-exprs (rest exprs) env))))

(define (check-let-expr bindings body env)
  (match bindings
    [(list (binding t n e) rest ...)
     (let ([t* (check-expr e env)])
       (unless (type-equal? t t*)
         (error (format "type error: in `let` expected ~a got ~a"
                        t t*)))
       (check-let-expr rest body (extend-env env n t)))]

    [(list (binding t n e))
     (let ([t* (check-expr e env)])
       (unless (type-equal? t t*)
         (error (format "type error: in `let` expected ~a got ~a"
                        t t*)))
       (check-expr body (extend-env env n t)))]))

(define (check-call-expr func args env)
  (match (check-expr func env)
    [(function-t ps ret)
     (let ([ts (foldl (λ (e es)
                        (cons (check-expr e env) es))
                      '() args)])
       (unless (equal? ps ts)
         (error (format "type error: ~a expects ~a, got ~a" func ps ts )))
       ret)]
    [_ (error (format  "type error: ~a is not a function type" func))]))


(define (check-program p)
  (define (collect forms env)
    (match forms
      ['() env]
      [(list (tl-annotate name type) rest ...)
       (collect rest
                (extend-env env name type))]
      [(list _ rest ...) (collect rest env)]))

  (let ([env* (collect p fresh-env)])
    (displayln env*)
    (map (λ (f) (check-top f env*))
         (filter tl-definition? p))
    p))

(provide check-program)


(check-program
 (parse-program
  '((: add (-> int int int))
    (define (add x y) (+ x y))
    (: main (-> unit unit))
    (define (main)
      (add 1 2)))))
