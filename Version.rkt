(load "simpleParser.scm")

; left operand
(define operandL cadr)

; right operand
(define operandR caddr)

; the name list (first list) in M_state
(define namelist car)

; the value list (second list) in M_state
(define valuelist cadr)

; interpret the parsing results
; input: filename
(define interpret
  (lambda (filename)
    (lookUp 'return (M_statement_list (parser filename) (newState)) )))

; create a new state: true, false, or undefined
(define newState
  (lambda ()
    '((true false return) (#t #f 'undefined))))

; look up if the variable is in the state binding pairs
; inputs: name of the variable, current state
(define lookUp
  (lambda (name state)
    (cond
     ((null? (car state)) 'undeclared)
     ((eq? (car (namelist state)) name) (car (valuelist state)))
     (else (lookUp name (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '()) ) )))))

; check if an variable is defined or not
; takes a name and a namelist as input and returns whether a name is inside it
(define declared?
  (lambda (name names)
    (cond
      ((null? names) #f)
      ((eq? (car names) name) #t)
      (else (declared? name (cdr names))))))

; check is an variable is initialized or not
; previous condition, the variable is declared
; takes a state and a name as input, check if the state of it is uninitialized
(define initialized?
  (lambda (name state)
    (cond
      ((eq? (car (namelist state)) name) (not (eq? 'uninitialized (car (valuelist state)))))
      (else (initialized? (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '()) ) )))))

; convert the parsing results to a statement list
; inputs: statement list, current state
(define M_statement_list
  (lambda (stmts state)
    (if (null? stmts)
        state
        (M_statement_list (cdr stmts) (M_state (car stmts) state) ))))

; operate a statement: variable declaration, assignment, return, if statement, or while statement
; store the binding pairs (name, value) in M_state, first list is name list, second list is value list
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

; operate a variable declaration
; inputs: statement, current state
(define M_declare
  (lambda (stmt state)
    (cond
      ((not (declared? (operandL stmt) (namelist state))) (error 'declaredTwiceVar "a variable can't be declared twice"))
      ((null? (cddr stmt)) (M_add (operandL stmt) 'uninitialized state))
      (else (M_add (operandL stmt) (M_value (operandR stmt) state) state))))) ; add name null?????????????????????????

;define M_assign
(define M_assign
  (lambda (stmt state)
    (cond
      ((not (declared? (operandL stmt) (namelist state))) (error 'undeclaredVar "a variable must be declared before assignment"))
      (else (M_update (operandL stmt) (M_value (operandR stmt) state) state)))))
      
;define M_return
(define M_return
  (lambda (stmt state)
    (cond
      ((not (declared? (operandL stmt) (namelist state))) (error 'undeclaredVar "a variable must be declared before returning it"))
      ((not (initialized? (operandL stmt) state)) (error 'uninitializedVar "a variable must be initialized before returning it"))
      ((eq? #t (M_value (operandL stmt) state)) (M_add 'return 'true state))
      ((eq? #f (M_value (operandL stmt) state)) (M_add 'return 'false state))
      (else (M_add 'return (M_value (operandL stmt) state) state)))))

;define M_if
(define M_if
  (lambda (stmt state)
    (cond
      ((not (declared? (operandL stmt) (namelist state))) (error 'undeclaredVar "a variable must be declared before its first use"))
      ((not (initialized? (operandL stmt) state)) (error 'uninitializedVar "a variable must be initialized before its first use"))
      ((M_value (operandL stmt) state) (M_state (operandR stmt) state)) ; if contidion is true, execute the then branch
      ((null? (cdddr stmt)) state) ; no else branch
      (else (M_state (cadddr stmt) state))))) ; execute the else branch
      
;define M_while
(define M_while
  (lambda (stmt state)
    (cond
      ((not (declared? (operandL stmt) (namelist state))) (error 'undeclaredVar "a variable must be declared before its first use"))
      ((not (initialized? (operandL stmt) state)) (error 'uninitializedVar "a variable must be initialized before its first use"))
      ((M_value (operandL stmt) state) (M_while stmt (M_state (operandR stmt) state))) ; if condition is true, execute the while loop
      (else state))))
      
;define M_value
(define operator car)
(define operandL cadr)
(define operandR caddr)

(define M_value
  (lambda (expr state)
    (cond
      ((number? expr) expr)    ; the base case is just returns the value if it's a number
      ((eq? (car expr) '+) (+ (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '-) (- (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '*) (* (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) 'x) (* (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '/) (quotient (M_value (operandL expr) state) (M_value (operandR expr) state)))
      ((eq? (car expr) '%) (remainder (M_value (operandL expr) state) (M_value (operandR expr) state)))
      (else (error "unknown operator:" (car expr))) )))


; Update the state for current variable
; Previous condition: the name has been declared and value will not be null
(define M_update
  (lambda (name value state)
    (cond
      ((null? value) state) ;assume no null value input 
      ((null? name) state)
      ((null? state) (M_add name value '()))
      ((and (null? (namelist state)) (null? (valuelist state))) (M_add name value '()))
      ((eq? name (car (namelist state)))
       (cons (namelist state)
             (cons (cons value
                         (cdr (valuelist state))) '())))
      (else (cons
             (cons
              (car (namelist state))
              (namelist (M_update name value
                                  (cons
                                   (cdr (namelist state))
                                   (cons (cdr (valuelist state))
                                         '())))))
             (cons
              (cons
               (car (valuelist state))
               (valuelist (M_update name value
                                    (cons (cdr (namelist state))
                                          (cons (cdr (valuelist state))
                                                '())))))
              '()))))))
   
; Add (name value) to the current state list
; Previous condition: name has been declared/name not null
(define M_add
    (lambda (name value state)
      (cond
        ((null? state) (cons (cons name '()) (cons (cons value '()) '()))) 
        (else
         (cons
          (cons name (namelist state))
          (cons (cons value (valuelist state)) '()))))))
