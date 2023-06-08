
;;LINEAR ITERATION vs RECURSION
;;; Exercise 1.11
;;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. 
;; Write a procedure that computes f by means of an iterative process.

(define (recur-fun n)
    (cond 
        ((< n 3) n)
        (else 
            (+ (recur-fun (- n 1)) (* 2 (recur-fun (- n 2))) (* 3 (recur-fun (- n 3)))))))

(define (fun n)
    (iter-fun 2 1 0 n))

(define (iter-fun a b c count)
    (cond
        ((= count 0) c)
        (else 
            (iter-fun (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
            
;; Excercise 1.12

(define (pascal r c)
    (cond
        ((or (= c 1) (= c r)) 1)
        (else
            (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))


;; Excercise 1.16 (TODO)

;; Excercise 1.29

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (simpsons-int f a b n)
    (define (h) (/ (- b a) n))
    (define (yk k) ((f (+ a (* k h)))))
    (define (simpsons-term k)
        (* (cond 
            ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            (else 2)) (yk k)))
    (* (/ h 3) (sum simpsons-term 0 inc n)))

;; Excercise 1.30

(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))


;; Excerise 1.32

(define (factorial n)
 (if (= n 0) 
     1
     (* n (factorial (- n 1)))))

(define (inc a) (+ a 1))

(define (identity a) (+ a 0))

(define (product func a next b)
    (if (> a b)
        1
        (* (func a) 
           (product func (next a) next b))))

(define (factorial-1 n)
    (product identity 1 inc n))

(define (product-2 func a next b)
    (define (iter a result)
        (if (> a b)
            result  
            (iter (next a) (* (func a) result))))
    (iter a 1))

(define (factorial-2 n)
    (product-2 identity 1 inc n))

;; Example 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess counter)
        (let ((next (f guess))
                (counter (+ counter 1)))
            (display counter)
            (newline)
            (if (close-enough? guess next)
                next
                (try next counter))))
    (try first-guess 0))

    (define (golden-ratio x)
        (fixed-point (lambda (y) (+ 1 (/ 1 x))) 1.0))

;; Excercise 1.36

(define (x-to-the-y y)
    (fixed-point (lambda (x) (/ (log y) (log x))) 10.0))


;; Excercise 1.37
;; a

(define (cont-frac n d k)
    (define (frac-recur i)
        (cond
            ((= i k) (/ (n i) (d i)))
            (else (/ (n i) (+ (d i) (frac-recur (+ 1 i)))))))
    (frac-recur 1))

;;b


    (define (cont-frac-iter n d k i result)
        (cond
            ((= i 0) result)
            (else (cont-frac-iter n d k (- i 1) (/ (n i) (+ (d i) result))))))

    (trace cont-frac-iter)

    (define (cont-frac-2 n d k)
        (cont-frac-iter n d k k 0))


;; Excercise 1.37

(define (e-approx k)
    (cond 
        ((= (remainder k 3) 2) (/ (+ k 1) 1.5))
        (else 1)))

(cont-frac (lambda (i) 1.0)
           (lambda (i) (cond 
                ((= (remainder i 3) 2) (/ (+ i 1) 1.5))
                (else 1)))
           3)

