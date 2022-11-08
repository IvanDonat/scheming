(define (monte-carlo P rand-arg num-trials)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0) (/ trials-passed num-trials))
              ((P (rand-arg)) (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else (iter (- trials-remaining 1) trials-passed))))
    (iter num-trials 0))

(define (in-circle? r)
    (lambda (point) 
            (let ((x (car point))
                  (y (cdr point)))
                 (<= (+ (* x x) (* y y)) (* r r)))))

(define (random-point-generator r)
    (lambda () (cons (random r) (random r))))

(define pi
    (let ((r 1000.0) (iters 100000))
         (/ (* (square (* 2 r)) (monte-carlo (in-circle? r) (random-point-generator r) iters)) (square r))))