
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
            
;; Exercise 1.12

(define (pascal r c)
    (cond
        ((or (= c 1) (= c r)) 1)
        (else
            (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))


;; Exercise 1.16 (TODO)

;; Exercise 1.29
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

;; Exercise 1.30
(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))


;; Exercise 1.32
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
            (display guess)
            ;; (display counter)
            (newline)
            (if (close-enough? guess next)
                next
                (try next counter))))
    (try first-guess 0))

    (define (golden-ratio x)
        (fixed-point (lambda (y) (+ 1 (/ 1 x))) 1.0))

;; Exercise 1.36
(define (x-to-the-x y)
    (fixed-point (lambda (x) (/ (log y) (log x))) 10.0))

(define (average-helper args sum count)
    (if (null? args) 
    (/ sum count)
    (average-helper (cdr args) (+ sum (car args)) (+ 1 count))))

(define (average . args)
    (average-helper args 0 0))

(define (sum-helper args)
    (if (null? args) 0
    (+ (car args) (sum-helper (cdr args)))))

(define (sum . args)
    (sum-helper args))

(define (x-to-the-x-damp y)
    (fixed-point (lambda (x) (average x (/ (log y) (log x)))) 10.0))

;; Exercise 1.37
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


;; Exercise 1.37
(define (e-approx k)
    (cond 
        ((= (remainder k 3) 2) (/ (+ k 1) 1.5))
        (else 1)))

(cont-frac (lambda (i) 1.0)
           (lambda (i) (cond 
                ((= (remainder i 3) 2) (/ (+ i 1) 1.5))
                (else 1)))
           3)

;; define average damping
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; why doesn't ((average-damp x-to-the-x) 1000) act the same as (x-to-the-x-damp 1000)
(define (square x)
    (* x x))
(define (cube x)
    (* x x x))

;; why does 1 vs 1.0 change the guess
(define (sqrt-damp-1 x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (sqrt-damp-2 x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define dx 0.00001)

(define (deriv g)
    (lambda (x) 
        (/ (- (g (+ x dx)) (g x)) 
            dx)))

(define (newton-transformation g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transformation g) guess))

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))

(define (sqrt-damp-3 x)
    (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

;;;Exercise 1.40
(define (cubic a b c)
    (lambda (x)
        (+ (* x x x) (* a x x) (* b x) c)))

;;; How would you do this with a lambda not naming the func
;(newtons-method (cubic 1 2 3) 1)

;(newtons-method (lambda (x)
;        (+ (* x x x) (* 1 x x) (* 2 x) 3)) 1)

;; QUESTION: How does the x get defined in lambda functions? can their be multiple args?
;; Exercise 1.41
(define (double g)
    (lambda (x)
        (g (g x))))

;(trace double)
(((double (double double)) inc) 5)

;;Exercise 1.42
(define (compose f g)
    (lambda (x)
        (f (g x))))

;;Exercise 1.43
(define (repeated f n)
    (define (repeated-inner f n counter)
        (cond
            ((= counter (- n 1)) (compose f f))
            (else (compose f (repeated-inner f n (+ counter 1))))))
    (repeated-inner f n 1))

;;; QUESTION: HOW DO I DO EX 1.42 with lambda (i dont understand soln below)

(define (repeated-1 f n)
    (lambda (x)
        (cond   
            ((= n 0) x)
            (else ((compose (repeated-1 f (- n 1)) f) x)))))

(define (repeated-2 f n)
    (cond
     ((= n 1) f)
     (else (compose f (repeated-2 f (- n 1))))))

; ((repeated-2 square 2) 5)

; Exercise 1.44

(define (smooth f)
    (lambda (x)
        (average (f x) (f (+ x dx)) (f (- x dx)))))

  ;; QUESTION: how to do n smooth fold 

  ;Exercise 1.45 TODO

  ;Exercise 1.46

  (define (iterative-improve good-enough? improve)
    (lambda (guess)
        (newline)
        (display guess)
        (cond
            ((good-enough? guess) guess)
            (else ((iterative-improve good-enough? improve) (improve guess))))))
            
    (define (sqrt-iterative-improve x)
        ((iterative-improve 
            (lambda (guess) (< (abs (- (square guess) x)) 0.001))
            (lambda (guess) (average guess (/ x guess)))) 1.0))
        
        




