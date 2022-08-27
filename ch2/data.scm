(define (list-ref items n)
    (if (= n 0) 
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define (extend l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (extend (cdr l1) l2))))

(define (last l)
    (if (null? (cdr l))
        (car l)
        (last (cdr l))))

(define (reverse l)
    (if (null? (cdr l))
        l
        (extend (reverse (cdr l)) (cons (car l) '()))))