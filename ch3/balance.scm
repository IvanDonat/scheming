; globally scoped
(define balance 100)
(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) 
               balance)
        "Insufficient funds"))

; internal
(define withdraw
    (let ((balance 100))
        (lambda (amount)
                (if (>= balance amount)
                    (begin (set! balance (- balance amount)) balance)
                    "Insufficient funds"))))

; independent withdraws
(define (make-withdraw balance)
    (lambda (amount)
         (if (>= balance amount)
             (begin (set! balance (- balance amount)) balance)
             "Insufficient funds")))

; stateful account
(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) 
            balance)
        "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: " m))))
    dispatch)

; accumulator
(define (make-accumulator value)
    (lambda (amount)
            (begin (set! value (+ value amount))
                   value))))

; how-many-calls
(define (make-monitored f)
    (let ((count 0))
         (define (mf arg)
            (cond ((eq? arg 'how-many-calls?) count)
                  ((eq? arg 'reset) (set! count 0) count)
                  (else (begin (set! count (+ count 1)) (f arg)))))
          mf))

