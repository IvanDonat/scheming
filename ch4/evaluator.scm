; CORE

(define (eval exp env)
    (cond
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: eval" exp))))

(define apply-in-underlying-scheme apply) ; saving a reference to underlying apply in scheme
(define (apply procedure arguments)
    (cond
        ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure) (eval-sequence (procedure-body procedure) (extend-environment (procedure-parameters procedure) arguments (procedure-environment procedure))))
        (else "Unknown procedure type: apply" procedure)))

; evaluates each operand and returns a list of corresponding values
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

; evaluates the predicate in the environment
; if true, evaluates the consequent, else alternative
(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

; evaluates a sequence of expressions in the environment,
; value returned is the value of the final expression
(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))

; finds value to be assigned by eval'ing it,
; and sets the value in the designated environment
(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
    'ok)

(define (eval-definition exp env)
    (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
    'ok)


; EXPRESSIONS

; only numbers and strings are self-evaluating
(define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

; variables are represented by sybols
(define (variable? exp) (symbol? exp))

; quotations have the form (quote <text>)
(define (quoted? exp)
    (tagged-list? exp 'quote))

(define (text-of-quotation exp)
    (cadr exp))

; tagged lists begin with the symbol
(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

; assignments have the form (set! <variable> <value>)
(define (assignment? exp)
    (tagged-list? exp 'set!))

(define (assignment-variable exp)
    (cadr exp))

(define (assignment-value exp)
    (cadddr exp))

; definitions have the form (define <variable> <value>),
; or the form (define (<var> <param_1> ... <param_n>) <body>).
; the latter is syntax sugar for (define (var) (lambda (<p_1> ... <p_n>) <body>))
(define (definition? exp)
    (tagged-list? exp 'define))

(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp) (cddr exp))))

; lambda expressions
(define (lambda? exp)
    (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
    (cadr exp))
(define (lambda-body exp)
    (cddr exp))

(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

; if: predicate, consequent, optional alternative
; no alternative? default to false
(define (if? exp)
    (tagged-list? exp 'if))

(define (if-predicate exp)
    (cadr exp))

(define (if-consequent exp)
    (caddr exp))

(define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

; begin packages a sequence of expressions into a single expression
(define (begin? exp)
    (tagged-list? exp 'begin))

(define (begin-actions exp)
    (cdr exp))

(define (last-exp? seq)
    (null? (cdr seq)))

(define (first-exp seq)
    (car seq))

(define (rest-exps seq)
    (cdr seq))

(define (sequence->exp seq)
    (cond ; transforms a sequence into a single expression
        ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
    (cons 'begin seq))

; cond expression is
(define (cond? exp)
    (tagged-list? exp 'cond))

(define (cond-clauses exp)
    (cdr exp))

(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
    (car clause))

(define (cond-actions clause)
    (cdr clause))

(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "else clause isn't last: cond-if" clauses))
                (make-if (cond-predicate first) 
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))


; a procedure application is a compound expression that is not a tagged list,
; i.e. none of the above
(define (application? exp)
    (pair? exp))

(define (operator exp)
    (car exp))

(define (operands exp)
    (cdr exp))

(define (no-operands? ops)
    (null? ops))

(define (first-operand ops)
    (car ops))

(define (rest-operands ops)
    (cdr ops))

; predicates
(define (true? x)
    (not (eq? x false)))

(define (false? x)
    (eq? x false))

; procedures
(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
    (cadr proc))

(define primitive-procedures
    (list
        (list 'append append)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
    (map car primitive-procedures))

(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args))

(define (make-procedure parameters body env)
    (list 'procedure parameters body env))

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (procedure-parameters p)
    (cadr p))

(define (procedure-body p)
    (caddr p))

(define (procedure-environment p)
    (cadddr p))

; ENVIRONMENT
; an environment is a list of frames, where the enclosing environment
; is the cdr of the list. the empty environment is the empty list
(define (enclosing-environment env)
    (cdr env))

(define (first-frame env)
    (car env))

(define the-empty-environment '())

; each frame is a pair of lists:
; a list of variables bound in the frame and a list of the associated values
(define (make-frame variables values)
    (cons variables values))

(define (frame-variables frame)
    (car frame))

(define (frame-values frame)
    (cdr frame))

(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (error "Arguments' lengths don't match" vars vals)))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond
                ((null? vars) (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond 
                ((null? vars) (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond
                ((null? vars) (add-binding-to-frame! var val frame))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame) (frame-values frame))))

; RUNNING the evaluator

(define (setup-environment)
    (let ((initial-env (extend-environment (primitive-procedure-names) (primitive-procedure-objects) the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))

(define the-global-environment
    (setup-environment))

(define input-prompt "> ")
(define output-prompt "= ")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
    (newline)
    (newline)
    (display string))

(define (announce-output string)
    (newline)
    (display string)
    (newline))

(define (user-print object)
    (if (compound-procedure? object)
        ; to not print environment for compound procedures
        (display (list 'compound-procedure (procedure-parameters object) (procedure-body object) '<env>))
        (display object)))
    
; (driver-loop)