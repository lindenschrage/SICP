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

(define n1 (make-rat-improved -1 2))
(define n2 (make-rat-improved -1 -2))
(define n3 (make-rat-improved 1 -2))
(define n4 (make-rat-improved 1 2))

; Exercise 2.1
(define (make-rat n d)
    (define g (gcd n d))
        (cond
           ( (= d 0) (error "The denominator can't be zero!"))
           ( (> (* n d) 0) (cons (abs (/ n g)) (abs (/ d g)) ) )
           ( (< (* n d) 0) (cons (* -1 (abs (/ n g))) (abs (/ d g)) ) ) ) ) 

; Exercise 2.2
(define (point x-point y-point)
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

(define p1 (point 1 2))
(define p2 (point 1 8))
(define seg (make-segment p1 p2))
(define mid (midpoint-segment seg))



