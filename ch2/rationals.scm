(define (add-rat x y)
    (make-rat  (+ (* (numer x) (denom y))
                  (* (numer y) (denom x)))
               (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

(define (normalise-sign x)
    (define (make-positive-rat x)
        (make-rat (abs (numer x)) (abs (denom x))))
    (define (positive? x) (>= x 0))
    (define (positive-rat? x)
        (equal? (positive? (numer x)) (positive? (denom x))))
    (define (negate x)
        (make-rat (- (numer x)) (denom x)))
    (define (sign-func x)
        (if (positive-rat? x)
            (lambda (x) x)
            (lambda  (x) (negate x))))
    ((sign-func x) (make-positive-rat x)))

(define (make-rat n d)
    (cons n d))

(define (numer x)
    (let ((g (gcd (car x) (cdr x))))
        (/ (car x) g)))

(define (denom x)
    (let ((g (gcd (car x) (cdr x))))
        (/ (cdr x) g)))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

