(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op
                        initial
                        (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (enumerate-interval lo hi)
    (if (> lo hi)
        '()
        (cons lo (enumerate-interval (+ lo 1) hi))))

(define empty-board '())

(define (add-row new-row rest-of-queens)
    (cons new-row rest-of-queens))

(define (safe? k positions)
    (define (exists? x l)
        (if (null? l)
            #f
            (or (= x (car l))
                (exists? x (cdr l)))))
    (define (repeat num times)
        (if (= times 0)
            '()
            (cons num (repeat num (- times 1)))))
    (define (update-attacked attacked)
        (let ((left (car attacked))
              (mid (car (cdr attacked)))
              (right (car (cdr (cdr attacked)))))
            (list (- left 1) mid (+ right 1))))
    (define (safe-rows? k positions attacked)
        (define (safe-row? pos attacked)
            (not (exists? pos attacked)))
        (if (= k 0)
            #t
            (and (safe-row? (car positions) attacked)
                 (safe-rows? (- k 1) (cdr positions) (update-attacked attacked)))))
    (let ((added-queen (car positions))
          (old-queens (cdr positions)))
        (safe-rows? (- k 1) old-queens (update-attacked (repeat added-queen 3)))))

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter (lambda (positions) (safe? k positions))
                    (flatmap (lambda (rest-of-queens) 
                                (map (lambda (new-row) (add-row new-row rest-of-queens))
                                     (enumerate-interval 1 board-size)))
                             (queen-cols (- k 1))))))
    (queen-cols board-size))

(queens 8)