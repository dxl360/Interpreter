;(load "simpleParser.scm")

; left operand
(define lop cadr)

; right operand
(define rop caddr)

; the name list (first list) in M_state
(define namelist car)

; the value list (second list) in M_state
(define valuelist cadr)

; interpret the parsing results
; input: filename
(define interpret
  (lambda (filename)
    (lookUp 'return (M_statement_list (parser filename) (newState)))))

; create a new state: true, false, or undefined
(define newState
  (lambda ()
    '((true false return) (#t #f 'undefined))))

; look up if the variable is in the state binding pairs
; inputs: name of the variable, current state
(define lookUp
  (lambda (name state)
    (cond
     ((null? (name)) state)
     ((null? (state)) 'undeclared)
     ((eq? (car (namelist state)) name) (car (valuelist state)))
     (else (lookUp name (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '()) ) )))))

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
;(define M_declare
; (lambda (stmt state)))
; TBD

;define M_assign

;define M_return

;define M_if

;define M_while

;define M_value


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
