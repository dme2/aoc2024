#lang racket

(define input (string-split
								(port->string
									(open-input-file "d2.txt")) "\n"))

(define (to-nums l)
	(map (lambda (x) (map string->number x)) l))

(define (to-lists s)
		(map (lambda (a) 
				 (string-split a " ")) s))

(define (dist a b)
	(abs (- a b)))

(define (check-dist a b)
	(if (<= (dist a b) 3)
		#t
		#f))

(define (apply-between func lst)
	  (if (or (empty? lst) (empty? (rest lst))) 
			      '()
						   (cons (func (first lst) (second lst))
												   (apply-between func (rest lst)))))

(define (check-inc a b) 
	(if (> b a)
		#t
		#f))

(define (check-dec a b)
	(if (< b a)
		#t
		#f))

(define (b-t-n bool)
	(if (eq? #t bool)
		1
		0))

(define (and-list bools)
	  (foldl (lambda (x acc) (and x acc)) #t bools))

(define (or-list bools)
	  (foldl (lambda (x acc) (or x acc)) #f bools))

(define (is-safe f1 f2 f3 l)
	(and
		(and-list (apply-between f1 l))
		(or
			(and-list (apply-between f2 l))
			(and-list (apply-between f3 l)))))

(define (remove-nth lst n)
	  (let loop ([i 0] [lst lst])
			    (cond [(= i n) (rest lst)]
								[else (cons (first lst)
														(loop (add1 i) (rest lst)))])))

(define (rem-comb-list lst l n)
	  (if (eq? n (length l))
			      '()
						 (cons (remove-nth l n)
												   (rem-comb-list lst l (+ n 1)))))
(define (is-safe-comb in) 
	(or-list 
		(map 
			(lambda (a) 
				(is-safe check-dist check-dec check-inc a))
			(cons in (rem-comb-list '() in 0)))))

; (rem-comb-list '() '(7 6 4 2 1) 0)
; (is-safe-comb '(7 6 4 2 1))
(define res (to-nums (to-lists input)))

;; first star
(foldl + 0 (map b-t-n (map (lambda (x) 
			 (is-safe check-dist check-dec check-inc x))
		 		res)))

;; second star
(foldl + 0 (map b-t-n
								(map
									(lambda (x)
										(is-safe-comb x))
									res)))

