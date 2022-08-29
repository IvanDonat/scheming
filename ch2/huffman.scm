; ('leaf symbol weight)
(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? obj) (eq? (car obj) 'leaf))
(define (symbol-leaf obj) (cadr obj))
(define (weight-leaf obj) (caddr obj))

; (left right symbols weight)
(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch-tree tree) (car tree))
(define (right-branch-tree tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (decode bits tree)
    (define (choose-branch bit branch)
        (cond ((= bit 0) (left-branch-tree branch))
              ((= bit 1) (right-branch-tree branch))
              (else (error "huh"))))
    (define (decode-1 bits branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair)) 
                        (make-leaf-set (cdr pairs))))))

(define (encode-symbol symbol tree)
    (define (contains-symbol? set)
        (if (null? set)
            false
            (if (eq? symbol (car set))
                true
                (contains-symbol? (cdr set)))))
    (define (choose branch)
        (if (leaf? branch) 
            '()
            (let ((left (left-branch-tree branch))
                  (right (right-branch-tree branch)))
              (cond ((contains-symbol? (symbols left)) (cons 0 (choose left)))
                    ((contains-symbol? (symbols right)) (cons 1 (choose right)))
                    (else (error "Unknown symbol: " symbol))))))
    (choose tree))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree) 
                (encode (cdr message) tree))))

(define (successive-merge set)
    (if (= (length set) 1)
        (car set)
        (successive-merge (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set)))))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

;
; 50s rocks songs alphabet
;
(define l 
    '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))
(define tree (generate-huffman-tree l))

(define message
    '(get a job sha na na na na na na na na
      get a job sha na na na na na na na na
      wah yip yip yip yip 
      yip yip yip yip yip
      sha boom))

(encode message tree)
(decode (encode message tree) tree)