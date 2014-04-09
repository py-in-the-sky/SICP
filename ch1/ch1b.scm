(define (square x) (* x x))


;; exercise 1.21
(define (smallest-divisor n)

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          ;(else (find-divisor n (+ test-divisor 1)))
          (else (find-divisor n (next test-divisor)))))

  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))


;; exercise 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)  ; (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start-integer number-of-primes)
  (cond ((= number-of-primes 0) (newline) (display "done!"))
        ((even? start-integer) (search-for-primes (+ 1 start-integer) number-of-primes))
        ;((prime? start-integer) (timed-prime-test start-integer)
        ((fast-prime? start-integer 1) (timed-prime-test start-integer)
                                (search-for-primes (+ 2 start-integer) (- number-of-primes 1)))
        (else (search-for-primes (+ 2 start-integer) number-of-primes))))


;; exercise 1.23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
;; using this in find-divisor (runtime O(sqrt(n))), we halve the search space
;; and hence the runtime is reduced by a factor of 1/sqrt(2)


;; exercise 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))  ; test: (a^n)%n == a
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;; exercise 1.25

(define (expmod base exp m)
  ;; almost identical to fast-expt, except for the additional "remainder ... m"
  ;; at each step back up the stack (linear recursion)
  ;; so for extremely large values of base, expmod-alt will divide the extremely
  ;; large base^exp by m
  ;; on the other hand, as long as base < m, expmod will never divide values
  ;; greater than m^2 by m
  ;; furthermore, because of "remainder ... m" at every step back up the stack,
  ;; expmod will never square a value greater than m, whereas fast-expt may end
  ;; up squaring very large values
  ;; so the operands for expmod are moderated by m, but the operands for expmod-alt
  ;; aren't moderated
  ;; in both expmod and expmod-alt, the stack size is proportional to log(exp)
  ;; therefore, if division and squaring are super-linear operations, we may see
  ;; a siginificant time difference between expmod and expmod-alt
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (expmod-alt base exp m)
  ;; for a**n % n, you'll first calculate a**n and then divmod this by n
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; from http://www.billthelizard.com/2010/02/sicp-exercise-125-closer-look-at-expmod.html
;; The important point is that the original expmod procedure uses successive
;; squaring to perform its computations without ever having to deal with numbers
;; larger than m. Simple arithmetic with very large numbers is much slower than
;; arithmetic with 32-bit integers. That's because the time complexity for arithmetic
;; operations is based on the number of bits in the operands. The intermediate
;; results during computation in the fast-expt procedure get very big, very quickly
;; (the final result is growing exponentially, after all). Since we're only interested
;; in the remainder, modular arithmetic provides a much sleeker solution, as explained
;; in the footnote.

;; from http://community.schemewiki.org/?sicp-ex-1.25
;; The modified version of expmod computes huge intermediate results.
;; Scheme is able to handle arbitrary-precision arithmetic, but arithmetic with
;; arbitrarily long numbers is computationally expensive. This means that we get
;; the same (correct) results, but it takes considerably longer.

;; from http://eli.thegreenplace.net/2007/07/09/sicp-section-126/
;; There is a huge flaw in Alyssa’s code. The call to fast-expt will result in huge
;; numbers being crunched. Curiously, this problem takes time to show in Common Lisp
;; which has built-in arbitrary precision arithmetic (huge integers). In many
;; languages, special libraries are required to handle integers larger than the
;; machine word size (typically 2^32 – 1 for unsigned integers on 32-bit machines).
;; Since prime tests are usually invoked on huge numbers (for example, the numbers
;; used in RSA are typically hundreds of digits long), it just won’t work. And if
;; it will, it will take very long time because bignum arithmetic takes much much
;; longer than ordinary arithmetic.
;; The expmod implementation provided by the authors, on the other hand, doesn’t
;; suffer from this problem. It uses a mathematical trick that keeps all numbers
;; it works on low.


;; exercise 1.27

(define (test-carmichael n)  ;; nlogn because we test n-2 values for a and expmod is logn
  (define (loop a)
    (cond ((= a 1) true)  ;; all tests have passed
          ((= (expmod a n n) a) (loop (- a 1)))  ;; keep testing
          (else false)))  ;; test has failed
  (loop (- n 1)))


;; exercise 1.28

(define (nontriv-rt b n)
  ;; return whether b is a non-trivial square root of 1 modulo n
  ;; that is, whether (b!=1 and b!=n-1) and (b^2=1 modulo n)
  (if (and (not (or (= b 1) (= b (- n 1)))) (= 1 (remainder (square b) n)))
      b
      0))

(define (expmod base exp m)
(cond ((= exp 0) 1)
      ((even? exp) (remainder (nontriv-rt (square (expmod base (/ exp 2) m)) m)
                              m))
      (else (remainder (* base (expmod base (- exp 1) m))
                       m))))

(define (miller-rabin n)
  (define (loop a)
    (cond ((= a 1) true)
          ((= (expmod a (- n 1) n) 1) (loop (- a 1)))
          (else false)))
  (loop (- n 1)))

;; TODO: fix miller-rabin; test on known primes and Carmichaels
