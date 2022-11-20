; cons-stream, stream-cdr, stream-car, and others are built-ins in Scheme
; implemented here as an exercise

; delay / force
(define (memoise proc)
    (let ((has-run? false) (result false))
        (lambda ()
            (if (not has-run?)
                (begin (set! result (proc))
                       (set! has-run? true)
                       result)
                result))))

(define (delay proc)
    (memoise (lambda () proc)))

(define (force proc)
    (proc))

; stream definitions
; e.g. (define s (cons-stream 1 (cons-stream 2 (cons-stream 3 (cons-stream 4 the-empty-stream)))))
(define the-empty-stream '())
(define stream-null? null?)

(define (cons-stream a b)
    (cons a (delay b)))

(define (stream-car s)
    (car s))

(define (stream-cdr s)
    (force (cdr s)))

; stream utils
(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

; e.g. (stream-map (lambda (x) (* 2 x)) s)
(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (stream-filter pred? stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred? (stream-car stream))
                  (cons-stream (stream-car stream)
                               (stream-filter pred? (stream-cdr stream))))
          (else (stream-filter pred? (stream-cdr stream)))))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each
        (lambda (x) (begin (display x) (newline)))
        s))