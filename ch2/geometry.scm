(define (make-point x y)
    (cons x y))

(define (row x) (car x))
(define (col x) (cdr x))

(define (print-point p)
    (newline)
    (display (row p))
    (display ",")
    (display (col p)))

; rect is represented by two opposing points
(define (make-rect a b)
    (cons a b))

(define (width rect)
    (define p1 (car rect))
    (define p2 (cdr rect))
    (abs (- (col p1) (col p2))))

(define (height rect)
    (define p1 (car rect))
    (define p2 (cdr rect))
    (abs (- (row p1) (row p2))))

(define (circum rect)
    (* 2 (+ (width rect) (height rect))))

(define (area rect)
    (* (width rect) (height rect)))