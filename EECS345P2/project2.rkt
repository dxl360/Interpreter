#| An interpreter for a Java-like language. Uses simpleParser.scm to parse input.
   Team member:  Yang Ding yxd155 / Guanzhou Qu gxq3 / Emilio Colindres exc231
|#

(load "simpleParser.scm")

; left operand
(define lop cadr)
; condition branch in if statements
(define condition lop)
; For M_state, the second list is the values corresponding to the variables
(define valueList cadr)

; right operand
(define rop caddr)
; while body in while statements
(define whilebody rop)
; then branch in if statements
(define thenbranch rop)

; else branch in if statements
(define elsebranch cadddr)

; the rest of the statements, used in Mstatement_list
(define nextStatements cdr)
; the body of a block
(define blockbody cdr)
; the remaining values of the valuelist in the state list      
(define remainingValues cdr)
; the first variable of the variablelist in the state list        
(define firstVariable car)
; the remaining variables of the variablelist in the state list         
(define remainingVariables cdr)
; the remaining layers in the state
(define remainingLayers cdr)

; the first value of the valuelist in the state list
(define firstValue car)
;operator
(define operator car)
; For M_state, the first list is the variables
(define variableList car)
; the first statement in the program, used in M_statement_list
(define firstStatement car)
; the first layer
(define firstLayer car)

; the initialization of a var
(define initialization cddr)

(define return-val
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      ((eq? val 'undeclared) (error "usage of undeclared variable"))
      ((eq? val 'uninitialized) (error "usage of uninitialized variable"))
      (else val))))

; the main function
(define interpret
  (lambda (filename)
    (return-val (call/cc
                 (lambda (return)
                   (M_state (parser filename) (newstate) return
                            (lambda (v) (error "invalid continue"))
                            (lambda (v) (error "invalid break"))
                            (lambda (v state) (error "unhandled exception" v))))))))



(define state-contains? (lambda (var state) (not (eq? 'undeclared (lookupList var state)))))

(define lookupList
  (lambda (var state)
    (cond
      ((null? state) 'undeclared)
      ((null? (firstLayer state)) (lookupList var (remainingLayers state)))
      ((not (eq? 'undeclared (lookup var (firstLayer state)))) (lookup var (firstLayer state)))
      (else (lookupList var (remainingLayers state))))))

; lookup a variable in the current state binding 
(define lookup
  (lambda (varName state)
    (cond
      ((or (null? state) (null? (variableList state))) 'undeclared)
      ((eq? varName (firstVariable (variableList state))) (firstValue (valueList state)))
      (else (lookup varName (cons (remainingVariables (variableList state)) (cons (remainingValues (valueList state)) '())))))))

(define atom? (lambda (stmt) (and (not (pair? stmt)) (not (null? stmt)))))
(define statement-list? (lambda (stmt) (list? (car stmt))))

(define op-equals (lambda (stmt op) (eq? op (operator stmt))))

(define assignment? (lambda (stmt) (op-equals stmt '=)))
(define declaration? (lambda (stmt) (op-equals stmt 'var)))
(define if? (lambda (stmt) (op-equals stmt 'if)))
(define while? (lambda (stmt) (op-equals stmt 'while)))
(define try? (lambda (stmt) (op-equals stmt 'try)))
(define catch? (lambda (stmt) (op-equals stmt 'catch)))
(define finally? (lambda (stmt) (op-equals stmt 'finally)))
(define block? (lambda (stmt) (op-equals stmt 'begin)))
(define return? (lambda (stmt) (op-equals stmt 'return)))
(define break? (lambda (stmt) (op-equals stmt 'break)))
(define continue? (lambda (stmt) (op-equals stmt 'continue)))
(define throw? (lambda (stmt) (op-equals stmt 'throw)))

; interprets a single statement to determine its state
(define M_state
  (lambda (stmt state return continue break throw)
    (cond
      ((null? stmt) state)
      ((atom? stmt) state)
      ((statement-list? stmt) (M_statement_list stmt state return continue break throw))
      ((assignment? stmt) (M_state_assign stmt state return continue break throw))
      ((declaration? stmt) (M_state_declare stmt state return continue break throw))
      ((if? stmt) (M_state_if stmt state return continue break throw))
      ((while? stmt) (M_state_while stmt state return continue break throw))
      ((try? stmt) (M_state_try stmt state return continue break throw))
      ((block? stmt) (M_state_block stmt state return continue break throw))
      ((return? stmt) (M_state_return stmt state return continue break throw))
      ((throw? stmt) (throw (cadr stmt) state))
      ((break? stmt) (break state))
      ((continue? stmt) (continue state))
      (else state))))

; divide parse tree to statements
(define M_statement_list
  (lambda (stmts state return continue break throw)
    (if (null? stmts)
        state
        (M_statement_list (nextStatements stmts) (M_state (firstStatement stmts) state return continue break throw) return continue break throw))))

(define M_state_block
  (lambda (stmt state return continue break throw)
    (removeLayer (M_statement_list (blockbody stmt) (addLayer state) return
                                   (lambda (s) (continue (removeLayer s)))
                                   (lambda (s) (break (removeLayer s)))
                                   (lambda (v s) (throw v (removeLayer s)))))))

; interprets an assign statement
(define M_state_assign
  (lambda (stmt state return continue break throw)
    (if (state-contains? (lop stmt) state)
        (addtostate (lop stmt) (M_value (rop stmt) state) state)
        (error 'variableUndeclared "assigning to a variable without having declared it first"))))

; interprets a declaration statement
(define M_state_declare
  (lambda (stmt state return continue break throw)
    (cond
      ((state-contains? (lop stmt) state) (error 'variableAlreadyDeclared "declaring a variable that has already been declared" (lop stmt)))
      ((null? (initialization stmt)) (addtostate (lop stmt) 'uninitialized state))
      (else (addtostate (lop stmt) (M_value (rop stmt) state) (M_state (rop stmt) state return continue break throw))))))

(define uninitialized? (lambda (stmt state) (eq? 'uninitialized (M_value (condition stmt) state))))
(define undeclared? (lambda (stmt state) (eq? 'undeclared (M_value (condition stmt) state))))
(define hasElseBranch? (lambda (stmt) (not (null? (cdddr stmt)))))

;interprets an if statement
(define M_state_if
  (lambda (stmt state return continue break throw)
    (cond
      ((undeclared? stmt state) (error 'variableUndeclared "undeclared variable in if condition"))
      ((uninitialized? stmt state) (error 'variableUninitialized "uninitialized variable in if condition"))
      ((M_value (condition stmt) state) (M_state (thenbranch stmt) state return continue break throw))
      ((hasElseBranch? stmt) (M_state (elsebranch stmt) state return continue break throw))
      (else state))))

(define tryBody cadr)

(define finallyBody
  (lambda (stmt)
    (cadr (cadddr stmt))))

(define catchBody
  (lambda (stmt)
    (cdr (cdaddr stmt))))

(define errorName
  (lambda (stmt)
    (caar (cdaddr stmt))))

(define M_state_try
  (lambda (stmt state return continue break throw)
      (M_state (finallyBody stmt) (M_state (tryBody stmt) state return continue break (lambda (e newstate)
                                                                                         (M_state (finallyBody stmt) (M_state_catch (catchBody stmt) e (errorName stmt) newstate return continue break throw) return continue break throw))) return continue break throw)))

(define M_state_catch
  (lambda (stmt error errorName state return continue break throw)
      (M_state stmt (addtostate errorName error state) return continue break throw)))

; interprets an return statement
(define M_state_return
  (lambda (stmt state return continue break throw)
    (return (M_value (lop stmt) state))))

; interprets a while statement
(define M_state_while
  (lambda (stmt state return continue break throw)
    (cond
      ((undeclared? stmt state) (error 'variableUndeclared "undeclared variable in while condition"))
      ((uninitialized? stmt state) (error 'variableUninitialized "uninitialized variable in if condition"))
      (else
       (call/cc
        (lambda (new-break)
          (letrec
              ((loop (lambda (test body state)
                       (if (M_value test state)
                           (loop test body (call/cc (lambda (new-continue)
                                                      (M_state body state return new-continue new-break throw))))
                           state))))
            (loop (condition stmt) (whilebody stmt) state))))))))

; returns the mathematical value of a statement
(define M_value
  (lambda (stmt state)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((not (list? stmt)) (lookupList stmt state))
      ((null? (cdr stmt)) (M_value (car stmt) state))
      ((or (eq? 'uninitialized (M_value (lop stmt) state)) (eq? 'undeclared (M_value (lop stmt) state))) (error "usage of variable without having declared or initialized"))
      ((and (not (null? (cddr stmt))) (or (eq? 'uninitialized (M_value (rop stmt) state)) (eq? 'undeclared (M_value (rop stmt) state)))) (error "usage of variable without having declared or initialized"))                                                                                                                                                                                                    
      ((eq? '+ (operator stmt)) (+ (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((and (eq? '- (operator stmt)) (null? (cddr stmt)))  (- (M_value (lop stmt) state)))
      ((eq? '- (operator stmt)) (- (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '* (operator stmt)) (* (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '/ (operator stmt)) (quotient (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '% (operator stmt)) (remainder (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '== (operator stmt))(= (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '!= (operator stmt))(not (= (M_value (lop stmt) state) (M_value (rop stmt) state))))
      ((eq? '< (operator stmt))(< (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '> (operator stmt))(> (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '<= (operator stmt))(<= (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '>= (operator stmt))(>= (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '|| (operator stmt))(or (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '&& (operator stmt))(and (M_value (lop stmt) state) (M_value (rop stmt) state)))
      ((eq? '! (operator stmt))(not(M_value (lop stmt) state)))
      (else (error "unknown expression")))))


(define layer-contains? (lambda (var layer) (not (eq? 'undeclared (lookup var layer)))))

(define addtostate
  (lambda (var value state)
    (if (eq? 'undeclared value)
        (error "usage of undeclared variable" var)
        (call/cc
         (lambda (escape)
           (addtostate-escape var value state (lambda () (escape (cons (addtolayer var value (firstLayer state)) (remainingLayers state))))))))))

(define addtostate-escape
  (lambda (var value state escape)
    (cond
      ((null? state) (escape))
      ((layer-contains? var (firstLayer state)) (cons (addtolayer var value (firstLayer state)) (remainingLayers state)))
      (else (cons (firstLayer state) (addtostate-escape var value (remainingLayers state) escape))))))

(define addtolayer
  (lambda (varName value state)
    (cond
      ((null? (variableList state))
       (cons (append (variableList state) (cons varName '())) (cons (append (valueList state) (cons value '())) '())))
      ((eq? (car (variableList state)) varName)
       (cons (variableList state)  (cons (cons value (cdr (valueList state))) '())) )
      (else (cons (cons (car (variableList state)) (car (addtolayer varName value (cons (cdr (variableList state))
                                                                                        (cons (cdr (valueList state)) '()))) )) (cons (cons (car (valueList state)) (car (cdr (addtolayer varName value
                                                                                                                                                                                          (cons (cdr (variableList state)) (cons (cdr (valueList state)) '()))) ))) '()) )))))

; the initial state
(define newstate
  (lambda ()
    '(((true false) (#t #f)))))

(define addLayer
  (lambda (state)
    (cons '(() ()) state)))

(define removeLayer
  (lambda (state)
    (cdr state)))

