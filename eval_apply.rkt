#lang sicp

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence
          (begin-actions exp)
          env))
        ((and? exp)
         (eval-and exp env))
        ((or? exp)
         (eval (or->if exp) env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp)))
        ((application? exp)
         (metacircular-apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression type: EVAL " exp))))

(define (metacircular-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters
            procedure)
           arguments
           (procedure-environment
            procedure))))
        (else
         (error "Unknown procedure type: METACIRCULAR-APPLY " procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values
             (rest-operands exps)
             env))))



(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                       env))))

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; Ex 4.1
(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons (first-value
               (list-of-values-l2r
                (rest-operands exps)
                env))))))
(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values
             (list-of-values-r2l
                (rest-operands exps)
                env)))
        (cons (eval (first-operand exps) env)
              rest-values))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp)
         true)
        ((eq? '*unassigned* exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)
       (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))


(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;ex 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (eval-and-conds conds)
    (let ((evaluated-cond (eval (car conds) env)))
      (if (true? evaluated-cond)
          (if (null? (cdr conds))
              evaluated-cond
              (eval-and-conds (cdr conds)))
          'false)))
  (if (null? (cdr exp))
      'true
      (eval-and-conds (cdr exp))))
(define (or->if exp)
  (define (or->if-conds conds)
    (if (null? conds)
        'false
        (make-if
         (car conds)
         (car conds)
         (or->if-conds (cdr conds)))))
  (or->if-conds (cdr exp)))

; -- end of ex 4.4

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
; Ex 4.5 is integrated
(define (cond-=>? clause)
  (and (= (length clause) 3)
       (eq? '=> (cadr clause))))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ;no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond
          ((cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't last: COND->IF " clauses)))
          ;((cond-=>? first) ; This works incorrectly if the condition has side-effects
          ; (make-if (car first)
          ;          (eval (list (caddr first) (car first)) env)
          ;          (expand-clauses rest)))
          (else (make-if (cond-predicate first)
                     (sequence->exp
                      (cond-actions first))
                     (expand-clauses rest)))))))

; Ex 4.6 and 4.8
(define (let->combination exp)
  (if (pair? (cadr exp))
      (let  ; ordinary let
          ((varexps (cadr exp))
           (body (cddr exp)))
        (cons
         (make-lambda
          (map car varexps)
          body)
         (map cadr varexps)))
      (let ; named let
          ((var (cadr exp))
           (bindings (caddr exp))
           (body (cadddr exp)))
        ; This DOES NOT WORK -> FIX IT
        (cons
         (make-lambda
          (map car bindings)
          body)
         (map cdr bindings)))))
        

; Ex 4.7
(define (let*->nested-lets exp)
  (let ((varexps (cadr exp))
        (body (caddr exp)))
    (if (null? varexps)
        body
        (list 'let
              (car varexps)
              (let*->nested-lets (list 'let* (cdr varexps) body))))))
; I don't think there's a need to explicitely expand the let.
; I should test this later when the whole thing seems to work

; Ex 4.9
; Syntax: (for (i '(1 2 3)) (print i))
; translate to:
; Seems very challenging to do this without nameclashing issues
  
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        (scan-out-defines body)
        env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))


(define (lookup-variable-value var env)
  (define (validate-var var val)
    (if (eq? val '*unassigned*)
        (error "Unassigned variable" var)
        val))
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (validate-var (car vars) (car vals)))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET! " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (scan-out-defines proc-body)
  (define (split-defines proc-body ret-defs ret-body)
    (cond ((null? proc-body) (cons ret-defs ret-body))
          ((definition? (car proc-body))
           (split-defines (cdr proc-body) (cons (car proc-body) ret-defs) ret-body))
          (else (split-defines (cdr proc-body) ret-defs (cons (car proc-body) ret-body)))))
  (let* ((defs-and-body (split-defines proc-body '() '()))
         (defs (car defs-and-body))
         (body (cdr defs-and-body)))
    (if (null? defs)
        proc-body
        (list (append
               (list 'let
                     (map (lambda (var) (list var '*unassigned*))
                          (map (lambda (def) (definition-variable def)) defs)))
               (map (lambda (def) (list 'set!
                                        (definition-variable def)
                                        (definition-value def)))
                    defs)
               body)))))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-eval input:")
(define output-prompt ";;; M-eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (eval input
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

(define the-global-environment
  (setup-environment))

(eval '(define (map op vals)
         (if (null? vals)
             vals
             (cons (op (car vals))
                   (map op (cdr vals)))))
      the-global-environment)

;(driver-loop)



