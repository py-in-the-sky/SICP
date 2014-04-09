(define (square x) (* x x))


;; Exercise 1.7
;; improve the sqrt function by making good-enough? dependent on the percentage
;; change in guess, therefore allowing to perform well at different scales
;; (e.g., for very small and very large numbers)

(define (sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
  (define (improve guess x) (average guess (/ x guess)))
  (define (average x y) (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (/ (abs (- guess (improve guess x))) guess)
       0.0000001))
  (sqrt-iter 1.0 x))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))


;; Exercise 1.8

(define (cbrt x)
  (define (cbrt-iter guess x)
    (if (good-enough-cbrt? guess x)
        guess
        (cbrt-iter (improve-cbrt guess x)
                   x)))
  (define (improve-cbrt guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough-cbrt? guess x)
    (< (/ (abs (- guess (improve-cbrt guess x))) guess)
       0.0000001))
  (cbrt-iter 1.0 x))
