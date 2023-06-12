; Representing rational numbers
(define (error str)
    (display str))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat-old n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(define (numer x)
    (car x))

(define (denom x)
    (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (if (= (/ (numer x) (denom x)) (/ (numer y) (denom y)))
        #t
        #f))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define one-fourth (make-rat 1 4))
(define two-fourths (make-rat 2 4))


; Exercise 2.1
(define (make-rat-improved n d)
    (define g (gcd n d))
        (cond
           ( (= d 0) (error "The denominator can't be zero!"))
           ( (> (* n d) 0) (cons (abs (/ n g)) (abs (/ d g)) ) )
           ( (< (* n d) 0) (cons (* -1 (abs (/ n g))) (abs (/ d g)) ) ) ) ) 

(define n1 (make-rat-improved -1 2))
(define n2 (make-rat-improved -1 -2))
(define n3 (make-rat-improved 1 -2))
(define n4 (make-rat-improved 1 2))

; Exercise 2.2
(define (make-point x-point y-point)
    (cons x-point y-point))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start-segment end-segment)
    (cons start-segment end-segment))

(define (midpoint-segment segment)
    (point (average (x-point (car segment)) (x-point (cdr segment)))
           (average (y-point (car segment)) (y-point (cdr segment)))))

;(define p1 (make-point 0 -5))
;(define p2 (make-point 4 2))
;(define seg (make-segment p1 p2))
;(define mid (midpoint-segment seg))


; Exercise 2.3
(define (make-rectangle bottom-left-p top-right-p)
    (cons bottom-left-p top-right-p))

(define (bottom-left-p rect)
    (car rect))

(define (top-right-p rect)
    (cdr rect))

(define (bottom-right-p rect)
    (make-point (x-point (top-right-p rect)) (y-point (bottom-left-p rect)))) 

(define (top-left-p rect)
    (make-point (x-point (bottom-left-p rect)) (y-point (top-right-p rect)))) 

(define (rec-height rect)
    (abs (- (y-point (bottom-left-p rect)) (y-point (top-left-p rect)))))

(define (rec-width rect)
    (abs (- (x-point (bottom-left-p rect)) (x-point (bottom-right-p rect)))))

(define (rec-perimeter rect)
    (+ (* 2 (rec-width rect)) (* 2  (rec-height rect))))

(define (rec-area rect)
    (* (rec-width rect) (rec-height rect)))

;(define rec (make-rectangle p1 p2))
;(define bl (bottom-left-p rec))
;(define tr (top-right-p rec))
;(define br (bottom-right-p rec))
;(define tl (top-left-p rec))

;QUESTION about 2.1.3 definition of cons

; Practice 
(define (cons-procedure x y)
    (define (dispatch m)
        (cond ((= m 0) x)
              ((= m 1) y)
              (else (error "not 0 or 1"))))
    dispatch)

(define (car-procuedure z) (z 0))
(define (cdr-procuedure z) (z 1))

;Exercise 2.4 (QUESTION)
(define (cons-2 x y)
    (lambda (m) (m x y)))

(define (car-2 z)
    (z (lambda (p q) p))) 

(define (cdr-2 z)
    (z (lambda (p q) q))) 

(trace cons-2)
(trace car-2)

;Exercise 2.5
(define (exp base n)
    (cond
        ((= n 0) 1)
        (else (* base (exp base (- n 1))))))

(define (divides? a b)
    (cond
        ((= 0 (remainder b a)) #t)
        (else #f)))

(define (logb base n)
    (/ (log n) (log base)))

(define (cons-int a b)
    (* (exp 2 a) (exp 2 b)))


;QUESTION: How could I make this an abstraction?
(define (car-int x)
    (cond  
        ((divides? 3 x) (car-int (/ x 3)))
        (else (logb 2 x))))

(define (cdr-int x)
    (cond  
        ((divides? 2 x) (cdr-int (/ x 2)))
        (else (logb 3 x))))

;Exercise 2.6
;QUESTION: What is going on
(define zero
    (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
    (lambda (f) (lambda (x) (f x))))