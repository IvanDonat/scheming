(define (sum term a next b)
    (if (> a b) 
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (sum-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc x) (+ x 1))

(define (one x) 1)
(define (id x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (sum-cubes a b)
    (sum cube a inc b))

(define (integral f a b dx)
    (* (sum f a (lambda (x) (+ x dx)) b) dx))

(define (accumulate combiner null-value term a next b)
    (define (iter cum a)
        (if (> a b)
            cum
            (iter (combiner (term a) cum) (next a))))
    (iter null-value a))

; f(x, y) := x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
(define (f x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
    (+ (* x (square a)) (* y b) (* a b))))


; fixed point
(define (fixed-point f initial-guess)
    (define tolerance 0.0001)
    (define (close-enough? a b)
        (< (abs (- a b)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try initial-guess))

(define (sqrt x)
    (define (average a b) (/ (+ a b) 2))
    (fixed-point
        (lambda (y) (average y (/ x y)))
        1.0))

(define (golden-ratio)
    (fixed-point
        (lambda (x) (+ 1 (/ 1 x)))
        1.0))