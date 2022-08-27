; f(n) = n  < 3 : n
;        n >= 3 : f(n-1) + 2f(n-2) + 3f(n-3)

(define (f n)
    (if (< n 3)
        n
        (+ 
            (f (- n 1))
            (* 2 (f (- n 2)))
            (* 3 (f (- n 3))))))

(define (f-it n)
    (define (iter n-2 n-1 curr i)
        (cond ((< n 3) n)
              ((= i n) curr)
              (else (iter 
                        n-1
                        curr
                        (+ (* 3 n-2) (* 2 n-1) curr)
                        (+ i 1)))))
    (iter 0 1 2 2))