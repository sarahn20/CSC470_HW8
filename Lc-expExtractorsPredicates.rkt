; Enviroment Initializers
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (name value env)
    (list 'extend-env name value env)))
(define extend-env*
  (lambda (lon lov env)
    (cond
      ((null? lon) env)
      (else (extend-env* (cdr lon) (cdr lov) (extend-env (car lon) (car lov) env))))))
(define get-name
  (lambda (env) (cadr env)))
(define get-value
  (lambda (env) (caddr env)))
(define get-env
  (lambda (env) (cadddr env)))
(define empty-env?
  (lambda (env) (eq? 'empty-env (car env))))
(define apply-env
  (lambda (var-name env)
    (cond
      ((empty-env? env) #f)
      (else
       (if (eq? var-name (get-name env))
           (get-value env)
           (apply-env var-name (get-env env)))))))
(define has-binding?
  (lambda (var-name env)
    (not (eq? (apply-env var-name env) #f))))
(define env (extend-env 'a 5 (extend-env 'b 7 (empty-env))))
(define empty-stack
  (lambda () '()))
; Grammar Constructors
(define var-exp
  (lambda (s)
    (list 'var-exp s)))
(define lambda-exp
  (lambda (s lc-exp)
    (list 'lambda-exp s lc-exp)))
(define app-exp
  (lambda (lambda-exp param-value)
    (list 'app-exp lambda-exp param-value)))
; Grammar Extractors
(define lc-exp->type
  (lambda (lc-exp)
    (car lc-exp)))
(define var-exp->var-name
  (lambda (var-exp)
    (cadr var-exp)))
(define lambda-exp->bound-var
  (lambda (lambda-exp)
    (cadr lambda-exp)))
(define lambda-exp->body
  (lambda (lambda-exp)
    (caddr lambda-exp)))
(define app-exp->rator
  (lambda (app-exp)
    (cadr app-exp)))
(define app-exp->rand
  (lambda (app-exp)
    (caddr app-exp)))
; Grammar Predicates
(define var-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp)'var-exp)))
(define lambda-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'lambda-exp)))
(define app-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'app-exp)))
