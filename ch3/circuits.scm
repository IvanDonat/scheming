; queue
; load definitions from queue.scm for queue utils

; func tools
(define (call-each procedures)
    (if (null? procedures) 
        'done
        (begin ((car procedures))
               (call-each (cdr procedures)))))

; logical operations
(define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal " s))))

(define (logical-and a b)
    (if (and (= a 1) (= b 1))
        1
        0))

(define (logical-or a b)
    (if (or (= a 1) (= b 1))
        1
        0))

; gates
(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
             (after-delay inverter-delay
                          (lambda () (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

(define (and-gate a1 a2 output)
    (define (and-action)
        (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
             (after-delay and-gate-delay
                          (lambda () (set-signal! output new-value)))))
    (add-action! a1 and-action)
    (add-action! a2 and-action)
    'ok)

(define (or-gate a1 a2 output)
    (define (or-action)
        (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
             (after-delay or-gate-delay
                          (lambda () (set-signal! output new-value)))))
    (add-action! a1 or-action)
    (add-action! a2 or-action)
    'ok)

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
         (or-gate a b d)
         (and-gate a b c)
         (inverter c e)
         (and-gate d e s)
         'ok))

; wires
(define (make-wire)
    (let ((signal-value 0)
          (action-procedures '()))
      (define (set-signal! new-value)
        (if (not (= signal-value new-value))
            (begin (set! signal-value new-value)
                   (call-each action-procedures)) 
            'done))
      (define (add-action! proc)
        (set! action-procedures (cons proc action-procedures))
        (proc))
      (define (dispatch m)
        (cond ((eq? m 'get-signal) signal-value)
              ((eq? m 'set-signal!) set-signal!)
              ((eq? m 'add-action!) add-action!)
              (else (error "Unknown operation: WIRE" m))))
      dispatch))

(define (get-signal wire)
    (wire 'get-signal))
(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value))
(define (add-action! wire proc)
    ((wire 'add-action!) proc))

; display action
(define (probe name wire)
    (add-action!
        wire
        (lambda () (newline)
                   (display name)
                   (display " ")
                   (display (current-time the-agenda))
                   (display "  New value = ")
                   (display (get-signal wire)))))

; agenda
(define (make-time-segment time queue)
    (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda)
    (cons 0 '()))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
    (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
    (set-cdr! agenda segments))
(define (first-segment agenda)
    (car (segments agenda)))
(define (rest-segments agenda)
    (cdr (segments agenda)))

(define (empty-agenda? agenda)
    (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (car segments)))))
    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
             (insert-queue! q action)
             (make-time-segment time q)))
    (define (add-to-segments! segments)
        (if (= (segment-time (car segments)) time)
            (insert-queue! (segment-queue (car segments)) action)
            (let ((rest (cdr segments)))
                 (if (belongs-before? rest)
                     (set-cdr! segments 
                               (cons (make-new-time-segment time action)
                                     (cdr segments)))
                      (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
         (if (belongs-before? segments)
             (set-segments! agenda 
                            (cons (make-new-time-segment time action) 
                                  segments))
             (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
         (delete-queue! q)
         (if (empty-queue? q)
             (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty")
        (let ((first-segment (first-segment agenda)))
             (set-current-time! agenda (segment-time first-segment))
             (front-queue (segment-queue first-segment)))))

; after-delay
(define (after-delay delay action)
    (add-to-agenda!
        (+ delay (current-time the-agenda))
        action
        the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
             (first-item)
             (remove-first-agenda-item! the-agenda)
             (propagate))))

; sample simulation
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5 )
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)
