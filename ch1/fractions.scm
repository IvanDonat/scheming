(define (cont-frac n d k)
    (define (step cum i)
        (define (frac cum i) (/ (n i) (+ (d i) cum)))
        (if (= i 1)
            (frac cum 1)
            (step (frac cum i) (- i 1))))
    (step 0 k))

; phi
(define (phi)
    (/ 1.0  (cont-frac  (lambda (i) 1.0) (lambda (i) 1.0) 100)))

; e
(define (e)
    (define (n _) 1.)
    (define (d i)
        (if (= (remainder i 3) 2)
            (* 2 (/ (+ i 1) 3))
            1))
    (define (e-2) (cont-frac n d 100))
    (+ (e-2) 2))

; tan
(define (tan x k)
    (define (n i)
        (if (= i 1)
            x
            (- (* x x))))
    (define (d i) (+ 1. (* 2 (- i 1))))
    (cont-frac n d k))
