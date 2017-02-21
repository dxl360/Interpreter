(load "simpleParser.scm")

; left operand
(define operandL cadr)

; right operand
(define operandR caddr)

; format of the state binding pairs: ((variable name list) (variable value list))
; example: '((x y) (1 3)) => x=1 y=3

; the name list (first list) in current state binding pairs
(define namelist car)

; the value list (second list) in current state binding pairs
(define valuelist cadr)

; interpret the parsing results, call helper functions lookUp, M_statement_list, parser, and newState
; input: filename
; return: processed result
(define interpret
  (lambda (filename)
    (lookUp 'return (M_statement_list (parser filename) (newState)) )))

; initialize null state '(()())
(define newState
  (lambda ()
    '(() ())))

; look up if the variable is in the state binding pairs
; inputs: name of the variable, current state
; return: the state of the variable
; example: lookUp x in state ((x y) (1 3))
; => (lookUp 'x '((x y) (1 3))) => 1
(define lookUp
  (lambda (name state)
    (cond
     ((null? (car state)) (error name "was undeclared"))
     ((eq? (car (namelist state)) name) (car (valuelist state)))
     (else (lookUp name (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '()) ) )))))

; check if an variable is defined or not
; inputs: variable name, name list
; return: t/f
; example: (declared? 'x '(()())) => #f
(define declared?
  (lambda (name names)
    (cond
      ((null? names) #f)
      ((eq? (car names) name) #t)
      (else (declared? name (cdr names))))))

; check is an variable is initialized or not
; Assumption: the variable is declared
; inputs: variable name, current state
; return: t/f
; example: (initialized? 'x '((x) (1))) => #t
; (initialized? 'x '((x) (uninitialized))) => #f
(define initialized?
  (lambda (name state)
    (cond
      ((eq? (car (namelist state)) name) (not (eq? 'uninitialized (car (valuelist state)))))
      (else (initialized? name (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '()) ) )))))

; convert the parsing results to a statement list, call helper functions declared? and M_state
; inputs: statement list, current state
(define M_statement_list
  (lambda (stmts state)
    (cond
      ((null? stmts) state)
      ((declared? 'return (namelist state)) state)
      (else (M_statement_list (cdr stmts) (M_state (car stmts) state) )))))

; operate a statement, call helper functions M_declare, M_assign, M_return, M_if, M_while
; store the binding pairs (name, value) in M_state, the first list is name list, the second list is value list
; inputs: statement list, current state
(define M_state
  (lambda (stmt state)
    (cond
      ((eq? 'var (car stmt)) (M_declare stmt state))
      ((eq? '= (car stmt)) (M_assign stmt state))
      ((eq? 'return (car stmt)) (M_return stmt state))
      ((eq? 'if (car stmt)) (M_if stmt state))
      ((eq? 'while (car stmt)) (M_while stmt state))
      (else state))))

; operate a variable declaration, call helper functions declared?, M_add, and M_assign
; inputs: statement, current state
(define M_declare
  (lambda (stmt state)
    (cond
      ((declared? (operandL stmt) (namelist state)) (error 'declaredTwiceVar "a variable can't be declared twice"))
      ((null? (cddr stmt)) (M_add (operandL stmt) 'uninitialized state))
      (else (M_assign stmt (M_add (operandL stmt) 'uninitialized state))))))
      ;(else (M_add (operandL stmt) (M_value (operandR stmt) state) state)))))

; operate an assignment declaration, call helper functions M_value, declared?, lookUp, and M_update
; inputs: statement, current state
(define M_assign
  (lambda (stmt state)
    (cond
      ((not (declared? (operandL stmt) (namelist state))) (error 'undeclaredVar "a variable must be declared before assignment"))
      ((and (list? (operandR stmt)) (eq? '= (car (operandR stmt)))) ; if there is nested assignment
       (let ([stmtlist (M_assign (operandR stmt) state)]) ; always operate the inner-most assignment first
         ; and update the current state
         (M_update (operandL stmt)                              ; name 
                   (lookUp (operandL (operandR stmt)) stmtlist) ; value
                   stmtlist)))                                  ; state
       ;(M_update (operandL stmt) (lookUp (operandL (operandR stmt)) (M_assign (operandR stmt) state)) (M_assign (operandR stmt) state)))
      (else (M_update (operandL stmt) (M_value (operandR stmt) state) state)))))
      
; operate a return declaration, call helper functions M_value and M_add
; inputs: statement, current state
(define M_return
  (lambda (stmt state)
    (cond
      ((eq? #t (M_value (operandL stmt) state)) (M_add 'return 'true state))
      ((eq? #f (M_value (operandL stmt) state)) (M_add 'return 'false state))
      (else (M_add 'return (M_value (operandL stmt) state) state)))))

; operate a if declaration, call helper functions M_value and M_state
; inputs: statement, current state
(define M_if
  (lambda (stmt state)
    (cond
      ((eq? #t (M_value (operandL stmt) state)) (M_state (operandR stmt) state)) ; if contidion is true, execute the then branch
      ((null? (cdddr stmt)) state) ; no else branch
      (else (M_state (cadddr stmt) state))))) ; execute the else branch
      
; operate a while declaration, call helper functions M_value and M_state
; inputs: statement, current state
(define M_while
  (lambda (stmt state)
    (cond
      ((M_value (operandL stmt) state) (M_while stmt (M_state (operandR stmt) state))) ; if condition is true, execute the while loop
      (else state))))

; check if the expression is a single variable of list of single variables
; input: expression
; return: t/f
; example: (variable? '((/ 2 1))) => #t, (variable? '(x)) => #t
(define variable?
  (lambda (expr)
    (cond
      ((null? expr) #t)
      ((not (list? expr)) #t) ;single variable
      ((not (pair? expr)) #t) ;empty list
      ((eq? (cdr expr) null) #t) ;list of single variable
      (else #f))))
    
; look up the value of the variable, call helper functions declared? and initialized?
; inputs: expression, current state
; return: expression value
(define variable
  (lambda (expr state)
    (cond
      ((null? expr) '())
      ((list? expr) (variable (car expr) state)) ;if the value is a list of single value '(x) or empty list
      ((number? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((not (declared? expr (namelist state))) (error 'undeclaredVar "a variable must be declared before its first use"))
      ((not (initialized? expr state)) (error 'uninitializedVar "a variable must be initialized before its first use"))
      (else (lookUp expr state)))))

; operate arithmatic and boolean expression, call helper functions variable? and variable
; inputs: expression, current state
; return: value of the expression
; example: (M_value '(% 3 2) '(() ())) => 1
(define M_value
  (lambda (expr state)
    (cond
      ; if expression is a variable, look up its value
      ((variable? expr) (variable expr state))
      ; if expression is not a variable
      ; operate arithmetic expressions
      ((eq? (car expr) '=) (M_value (operandL (operandR expr)) state))
      ((and (eq? (car expr) '-) (null? (cddr expr)))  (- (M_value (operandL expr) state))) ; negative number
      ((eq? (car expr) '+) (+ (M_value (operandL expr) state) (M_value (operandR expr) state))) 
      ((eq? (car expr) '-) (- (M_value (operandL expr) state) (M_value (operandR expr) state))) 
      ((eq? (car expr) '*) (* (M_value (operandL expr) state) (M_value (operandR expr) state))) 
      ((eq? (car expr) 'x) (* (M_value (operandL expr) state) (M_value (operandR expr) state))) 
      ((eq? (car expr) '/) (quotient (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '%) (remainder (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ; operate booealn expressions
      ((eq? (car expr) '==) (= (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '!=) (not (= (M_value (operandL expr) state) (M_value (operandR expr) state))))
      ((eq? (car expr) '<) (< (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '>) (> (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '<=) (<= (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '>=) (>= (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '||) (or (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '&&) (and (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '!) (not (M_value (operandL expr) state)))
      ; unknown operator
      (else (error "unknown operator:" (car expr))) )))

; Update the state for current variable, call helper function M_add
; Previous condition: the name has been declared and value will not be null
; inputs: variable name, variable value, current state
; return: updated state
(define M_update
  (lambda (name value state)
    (cond
      ((null? value) state) ;assume no null value input 
      ((null? name) state)
      ((null? state) (M_add name value '()))
      ((and (null? (namelist state)) (null? (valuelist state))) (M_add name value '()))
      ((eq? name (car (namelist state))) ; if the first name in namelist equals to the variable name
       (cons (namelist state)            
             (cons (cons value           ; append the variable value to the valuelist starting from second value
                         (cdr (valuelist state))) '())))
      (else (cons                        ; if the first name in name list does not equal to the variable name
             (cons
              (car (namelist state))
              (namelist (M_update name value ; recursively call M_update to check if the first name of the sub-namelist equal to the variable
                                  (cons      
                                   (cdr (namelist state))
                                   (cons (cdr (valuelist state))
                                         '())))))
             (cons
              (cons
               (car (valuelist state))
               (valuelist (M_update name value ; recursively call M_update to append the variable value to the sub-valuelist starting from second value
                                    (cons (cdr (namelist state))
                                          (cons (cdr (valuelist state))
                                                '())))))
              '()))))))
   
; Add (name value) to the current state list
; Previous condition: name has been declared/name not null
; inputs: variable name, variable value, current state
; return: updated state
(define M_add
    (lambda (name value state)
      (cond
        ((null? state) (cons (cons name '()) (cons (cons value '()) '()))) 
        (else
         (cons
          (cons name (namelist state))
          (cons (cons value (valuelist state)) '()))))))
