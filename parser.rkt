#lang racket

(require racket/file)
(require racket/pretty)

(define nil 'nil)

(define zero 
  (λ (inp) 'nil ))

(define item 
  (λ (inp)
	(if (string=? inp "")
  		'nil
		(cons (string-ref inp 0)
					(substring inp 1)))))

(define (fmap f p)
  (λ (inp)
	(match (p inp)
	  [(cons r rs) (cons (f r) rs)]
	  ['nil nil])))

(define (result v)
  (λ (inp) 
	(cons v inp)))

(define (ap p q)
  (λ (inp)
	(match (p inp)
	  [(cons f rst) (match (q rst) 
					  ['nil nil]
					  [(cons v rst*) (cons (f v) rst*)])]
	  ['nil nil])))

(define (bind p f)
  (λ (inp)
	(match (p inp)
	  [(cons r rst) ((f r) rst)]
	  ['nil nil])))

(define (sequence ps)
  (foldr (λ (mx acc)
			(ap (fmap (curry cons) mx) acc))
		 (result '())
		 ps))

(define (either p . ps) 
  (λ (inp) 
	(let loop ([ps (cons p ps)])
	  (if (empty? ps) 
		  'nil
		  (match ((car ps) inp)
			[(cons r rst) (cons r rst)]
			[_ (loop (cdr ps))])))))

(define (left p q)
  (λ (inp) 
	(match (p inp)
	  ['nil 'nil]
	  [(cons r rst) (match (q rst)
					  ['nil 'nil]
					  [(cons _ rst*) (cons r rst*)])])))

(define (right p q)
  (λ (inp) 
	(match (p inp)
	  ['nil 'nil]
	  [(cons r rst) (q rst)])))

(define (some p)
  (λ (inp) 
	(let loop ([inp inp]
			   [result '()])
		(match (p inp)
		  ['nil (if (empty? result)
					'nil 
					(cons (reverse result) inp))]
		  [(cons r rst) (loop rst (cons r result))]))))


(define (many p)
  (either (some p) (result '())))


(define (sep-p sep p) 
	(ap 
	  (fmap (curry cons) p) 
	  (many (right sep p))))

(define (satisfy pred) 
  (bind item 
		(λ (x) 
		  (if (pred x) 
			  (result x)
			  zero))))

(define (char-p c)
  (satisfy (λ (x) (char=? c x))))

(define (string-p s) 
  (fmap list->string
		(sequence 
		  (map char-p (string->list s)))))

(define (span-p pred)
  (λ (inp) 
	(define-values (left right)
	  (splitf-at (string->list inp) pred))
	(if (empty? left)
		'nil
		(cons left (list->string right)))))

(define ws 
  (many (span-p char-whitespace?)))


;Json Parser
(define json-value
  (λ (inp) 
	((either 
	   json-null 
	   json-bool
	   json-number
	   json-string 
	   json-array 
	   json-object) inp)))

(define json-null
  (fmap string->symbol (string-p "null")))

(define json-bool
  (fmap string->symbol (either (string-p "true") (string-p "false"))))

; Add float
(define json-number 
  (fmap (compose string->number list->string)
		(span-p char-numeric?)))

; Add escaping
(define json-string
  (fmap
	list->string 
	(left (right (char-p #\")
				 (span-p 
				   (compose not ((curry char=?) #\"))))
		  (char-p #\"))))



(define json-array
  (let ([elem (sep-p (left (right ws (char-p #\,)) ws)
					   json-value)])
	(left (right (sequence (list (char-p #\[) ws))
				 (either elem (result '())))
		  (sequence (list ws (char-p #\]))))))


(define json-object
  (let* ([sep (sequence (list ws (char-p #\:) ws))]
		 [pair (sequence (list (left json-string sep) json-value))])
	(left (right (sequence (list (char-p #\{) ws))
		   (sep-p (sequence (list ws (char-p #\,) ws)) pair))
		  (sequence (list ws (char-p #\}))))))


(define (build-ht pairs)
  (define ht (make-hash))
  (let loop ([ps pairs])
	(match ps
	  ['() ht]
	  [(cons (list k v) rst) 
			 (begin (hash-set! ht k (match v 
									  [(cons (list k* v*) rst*) (build-ht v)]
									  [_ v]))
					(loop rst))])))

(define json-parser
  (λ (inp) 
	(match (json-value inp)
	  [(cons r "") (build-ht r)]
	  [_ 'nil])))

(module+ main 
	(define js-data 
	   (json-parser (string-trim (file->string "file.json"))))
	(pretty-print js-data)
  )



