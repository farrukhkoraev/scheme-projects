#lang racket 

(module maybe racket/base
  (require (only-in racket/match match)
		   (only-in racket/function curry))

  (struct nothing () #:transparent)
  (struct just (value) #:transparent)

  (define maybe?
	(λ (x) 
	  (or (nothing? x) 
		 (just? x))))

  (define (fmap f mb)
	(match mb
	  [(just x) (just (f x))]
	  [_ (nothing)]))
  (define <$> fmap)

  ; applicative
  (define (<*> mf mx)
	(match mf
	  [(just f) (fmap f mx)]
	  [_ (nothing)]))

  (define (pure x)
	 (just x))

  ; monad interface
  (define (>= mx f) 
	(match mx
	  [(nothing) (nothing)]
	  [(just x) (f x)]))

  (define mb-add1 
	(λ (x) (>= (pure x) 
			   (λ (x) (pure (add1 x))))))

  (define mb+
	(λ (x y . rst)
	  (define (f x acc) (<*> (<$> (curry +) x) acc))
	  (foldl f (f x y) rst)))

  (provide (all-defined-out))
  )

(module list-m racket/base
  (require (only-in racket/match match)
		   (only-in racket/function curry)
		   (only-in racket/list flatten))

  (define (fmap f xs)
	(match xs
	  ['() '()]
	  [ _  (map f xs)]))

  (define <$> fmap)

  ; applicative
  (define (<*> fs xs)
	  (for/list ([f fs] [x xs])
		(f x)))

  (define (pure x) (list x))

  ; monad interface
  (define (>= xs f) 
	(foldl (λ (x acc) (append acc x))
		   '()
		   (map f xs)))


  (provide (all-defined-out))
  )

(define-syntax do
  (syntax-rules ()
	 [(_ b-fn action) action]
	 [(_ b-fn [x action-1] action-2 ... action-n)
			  (b-fn action-1 (λ (x)
					  (do b-fn action-2 ... action-n)))]))


(module* main #f
   (require (prefix-in mb: (submod ".." maybe))
			(prefix-in ls: (submod ".." list-m)))


  (do mb:>=
  [x (mb:just 1)]
  [y (mb:just 2)]
  (* x y))

  (do ls:>= 
	[xs (list 1 2 3)]
	[ys (list 2 3 4)]
	(ls:pure (cons xs ys))))

