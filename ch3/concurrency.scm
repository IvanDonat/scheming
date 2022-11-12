; cell is a single element list which holds true or false
; and represents whether the mutex is available to be acquired
;   false -- available to be acquired
;   true -- mutex is unavailable
(define (test-and-set! cell)
    ; without-interrupts disables time slicing while its procedure is being executed
    (without-interrupts
        (lambda ()
                (if (car cell)
                    true
                    (begin (set-car! cell true) false)))))

(define (clear! cell)
    (set-car! cell false))

(define (make-mutex)
    (let ((cell (list false)))
        (define (acquire)
            (if (test-and-set! cell) (acquire))) ; retry until acquired
        (define (mutex message)
            (cond ((eq? message 'acquire) (acquire))
                  ((eq? message 'release) (clear! cell))))
        mutex))