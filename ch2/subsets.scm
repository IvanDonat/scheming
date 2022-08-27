(define x (list 1 2 3 4))

(define (subsets l)
    (if (null? l)
        (list '())
        (let ((subsets-rest (subsets (cdr l)))
              (first (car l)))
             (append subsets-rest
                     (map (lambda (s) (cons first s)) subsets-rest)))))

(subsets x)