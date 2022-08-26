(define (factorial n)
	(cond
		((= 0 n) 1)
		(else (* (factorial (- n 1)) n))))

(define (factorial n)
    (define (fact-iter product counter max-count)
        (if (> counter max-count)
            product
            (fact-iter (* product counter)
                    (+ counter 1)
                    max-count)))
    (fact-iter 1 1 n))
    