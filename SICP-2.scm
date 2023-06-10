; Representing rational numbers

(define (error str)
    (display str))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
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

(define (make-rat-improved n d)
    (define g (gcd n d))
        (cond
           ( (= d 0) (error "The denominator can't be zero!"))
           ( (> (* n d) 0) (cons (abs (/ n g)) (abs (/ d g)) ) )
           ( (< (* n d) 0) (cons (* -1 (abs (/ n g))) (abs (/ d g)) ) ) ) ) 

