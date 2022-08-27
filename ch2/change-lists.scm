(define (change amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+
                    (change amount (except-first coin-values))
                    (change (- amount (first coin-values)) coin-values)))))

(define no-more? null?)
(define except-first cdr)
(define first car)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))