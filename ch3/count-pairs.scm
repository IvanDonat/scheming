; Procedure that counts number of pairs in a structure
; Intentionally incorrect, double counts pairs it sees twice
(define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))

; count-pairs will return 3 for these three pairs
(define (3-pairs)
    '(1 2 3))

; count-pairs will return 4 for these three pairs
(define (4-pairs)
    (define B (cons 3 4))
    (define C (cons 1 B))
    (define A (cons C B))
    A)

; loop, count-pairs will exceed max recursion depth
(define (never-return)
    (define A '(1 2 3))
    (set-cdr! (cddr A) A)
    A)

; count-pairs returns 7 for 3 pairs
(define (7-pairs)
    (define C (cons 1 2))
    (define B (cons C C))
    (define A (cons B B))
    A)

; count-pairs-dedup
(define (count-pairs-dedup x)
    (let ((seen '()'))
        (define (count-pairs-dedup x) ; internal, with seen state
        (define (mark-seen x)
            (begin (set! seen (cons x seen))
                seen))
        (define (seen? x)
            (define (seen-iter? x seen)
                (cond ((not (pair? seen)) #f)
                    ((eq? (car seen) x) #t)
                    (else (seen-iter? x (cdr seen)))))
            (seen-iter? x seen))
        (if (or (not (pair? x)) (seen? x))
            0
            (begin
                (mark-seen x)
                (+ (count-pairs-dedup (car x))
                    (count-pairs-dedup (cdr x))
                    1))))))

; Floyd's algorithm for detecting cycles
(define (cycles? l)
    (define (move slow fast)
        (cond ((not (pair? fast)) #f)
              ((eq? slow fast) #t)
              (else (if (pair? (cdr fast))
                        (move (cdr slow) (cddr fast))
                        #f))))
    (move l (cdr l)))