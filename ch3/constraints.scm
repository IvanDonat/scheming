; utils
(define (for-each-except exception procedure list)
    (define (loop items)
        (cond ((null? items) 'done)
              ((eq? (car items) exception) (loop (cdr items)))
              (else (procedure (car items)) (loop (cdr items)))))
    (loop list))

; connectors
(define (make-connector)
    (let ((value false)
          (informant false)
          (constraints '()))
        (define (set-my-value new-value setter)
            (cond ((not (has-value? me))
                        (set! value new-value)
                        (set! informant setter)
                        (for-each-except setter inform-about-value constraints))
                    ((not (= value new-value))
                        (error "Contradiction" (list value new-value)))
                    (else 'ignore)))
        (define (forget-my-value retractor)
            (if (eq? retractor informant)
                (begin (set! informant false)
                       ; (set-value! false) -- no need, false informant implies no value
                       (for-each-except retractor inform-about-no-value constraints))
                'ignored))
        (define (connect constraint)
            (if (not (memq constraint constraints))
                (set! constraints (cons constraint constraints)))
            (if (has-value? me)
                (inform-about-value constraint))
            'done)
        (define (me request)
            (cond ((eq? request 'has-value?) (if informant true false))
                  ((eq? request 'value) value)
                  ((eq? request 'set-value!) set-my-value)
                  ((eq? request 'forget) forget-my-value)
                  ((eq? request 'connect) connect)
                  (else (error "Unknown request: make-connector" request))))
        me))

; syntax sugar to think of the connector procedure as data
(define (has-value? connector)
    (connector 'has-value?))
(define (get-value connector)
    (connector 'value))
(define (set-value! connector new-value informant)
    ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
    ((connector 'forget) retractor))
(define (connect connector constraint)
    ((connector 'connect) constraint))

; constraints
(define (constant value connector)
    (define (me request)
        ; constant connector errors or any request
        (error "Unknown request: constant" request))
    (connect connector me)
    (set-value! connector value me)
    me)

(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2)) 
                    (set-value! sum (+ (get-value a1) (get-value a2)) me))
              ((and (has-value? a1) (has-value? sum))
                    (set-value! a2 (- (get-value sum) (get-value a1)) me))
              ((and (has-value? a2) (has-value? sum))
                    (set-value! a1 (- (get-value sum) (get-value a2)) me))))
    (define (process-forget-value)
        ; only forget values set by this adder
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))
    (define (me request)
        (cond ; have-value is called to inform that one of the connectors has a value
              ((eq? request 'have-value) (process-new-value))
              ; lost-value is called when one of the connectors lost its value
              ((eq? request 'lost-value) (process-forget-value))
              (else (error "Unknown request: adder" request))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me) ; the procedure `me` is the adder

(define (multiplier m1 m2 product)
    (define (process-new-value)
        (cond ((and (has-value? m1) (has-value? m2)) 
                    (set-value! product (* (get-value m1) (get-value m2)) me))
              ((and (has-value? m1) (has-value? product))
                    (set-value! m2 (/ (get-value product) (get-value m1)) me))
              ((and (has-value? m2) (has-value? product))
                    (set-value! m1 (/ (get-value product) (get-value m2)) me))))
    (define (process-forget-value)
        ; only forget values set by this multiplier
        (forget-value! product me)
        (forget-value! m1 me)
        (forget-value! m2 me)
        (process-new-value))
    (define (me request)
        (cond ; have-value is called to inform that one of the connectors has a value
              ((eq? request 'have-value) (process-new-value))
              ; lost-value is called when one of the connectors lost its value
              ((eq? request 'lost-value) (process-forget-value))
              (else (error "Unknown request: multiplier" request))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me) ; the procedure `me` is the multiplier

(define (probe name connector)
    (define (print value)
        (newline)
        (display "Probe: ")
        (display name)
        (display " = ")
        (display value))
    (define (process-new-value)
        (print (get-value connector)))
    (define (process-forget-value)
        (print "None?"))
    (define (me request)
        (cond ((eq? request 'have-value) (process-new-value))
              ((eq? request 'lost-value) (process-forget-value))
              (else (error "Unknown request: probe" request))))
    (connect connector me)
    me)

; syntax sugar to think of the constraint procedures as data
(define (inform-about-value constraint)
    (constraint 'have-value))
(define (inform-about-no-value constraint)
    (constraint 'lost-value))

; celsius fahrenheit converter
(define (celsius-fahrenheit-converter C F)
    (let ((w (make-connector))
          (u (make-connector))
          (v (make-connector))
          (x (make-connector))
          (y (make-connector)))
        (multiplier C w u)
        (multiplier v x u)
        (adder v y F)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)
        'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius" C)
(probe "Fahrenheit" F)

; sample interaction
(set-value! F 32 'user)
; prints F=32, C=0

(set-value! C 100 'a)
; errors, contradiction

(forget-value! C 'other)
; ignores unset as it is not by user

(forget-value! F 'user)
; prints F and C values are none

(set-value! C -40 'a)
; prints C and F are -40