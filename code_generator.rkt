#lang racket
(require "parser.rkt")
(require "typechecker.rkt")


(define nested #false)

(define (make-running-total)
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))

(define retval_count (make-running-total))

(define generate_Retval (string-append "Retval" (~a (retval_count))))

(define (unwrap exp_or_stmt) (ParseResult-result exp_or_stmt))

(define closure_rule (let ([retval1 generate_Retval] [retval2 generate_Retval] [retval3 generate_Retval]) (string-join (list "call_closure(" retval1 ", " retval2 ") :- copy_term("retval1 ", (" retval2 " :- " retval3")), call(Body)."))))

(define switch_rule (let ([retval1 generate_Retval] [retval2 generate_Retval] [retval3 generate_Retval] [retval4 generate_Retval]) (string-join (list "switch(" retval1 ", [" retval2 ":" retval3 "|" retval4 "]) :- ((" retval1 "=" retval2 " ; " retval2 " == \"default\") -> call(" retval3 ") ; switch(" retval1 ", " retval4 ")).") "")))



(define (generate_assignment_statement name exp)
  (cond
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (string-join (list name " is " (generate_additive_or_multiplicative_expression exp))"")]
    [(Integer_Expression? exp) (string-join (list name " is " (generate_expression exp)))]
    [(Call_Expression? exp) (let ([retval generate_Retval]) (string-join (list (generate_call_expression (unwrap (Call_Expression-identifier exp)) (unwrap (Call_Expression-arguments exp)) retval) ", " name " = " retval) ""))]
    [(Function_Expression? exp ) (let ([retval generate_Retval]) (string-join (list (generate_clause (unwrap (Function_Expression-identifier exp)) (unwrap (Function_Expression-parameters exp)) (unwrap (Function_Expression-body exp)) (unwrap (Function_Expression-returned exp)) generate_Retval) ", " retval " = " retval) ""))]
    [else (string-join (list name " = " (generate_expression exp)))]))

(define (generate_additive_or_multiplicative_expression exp)
  (if (Additive_Expression? exp)
      (string-join (list (generate_expression (unwrap (Additive_Expression-primary1 exp))) (Additive_Expression-operand exp) (generate_expression (ParseResult-result (Additive_Expression-primary2 exp))))"")
      (string-join (list (generate_expression (unwrap (Multiplicative_Expression-primary1 exp))) (check_for_division (unwrap (Multiplicative_Expression-operand exp))) (generate_expression (unwrap (Multiplicative_Expression-primary2 exp))))"")))


(define (check_for_division operand)
  (if (equal? operand "/") "div" operand))


(define (generate_expression exp)
  (cond
    [(Integer_Expression? exp) (Integer_Expression-value exp)]
    [(String_Expression? exp) (string-join (list "\"" (String_Expression-value exp) "\"")"")]
    [(Boolean_Expression? exp) (Boolean_Expression-value exp)]
    [(Variable_Expression? exp) (string-titlecase (Variable_Expression-value exp))]
    [(Boolean_Operation_Expression? exp) (string-join (list (generate_expression (unwrap (Boolean_Operation_Expression-primary1))) (unwrap (Boolean_Operation_Expression-operand exp)) (generate_expression (unwrap (Boolean_Operation_Expression-primary2 exp))))"")]
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (generate_additive_or_multiplicative_expression exp)]
    [(Call_Expression? exp) (generate_call_expression (Variable_Expression-value (unwrap (Call_Expression-identifier exp))) (unwrap (Call_Expression-arguments exp)) generate_Retval)]
    [(Function_Expression? exp) (generate_clause (Variable_Expression-value (unwrap (Function_Expression-identifier exp))) (unwrap (Function_Expression-parameters exp)) (unwrap (Function_Expression-body exp)) (unwrap (Function_Expression-returned exp)) generate_Retval)]
    [(If_Expression? exp) (generate_if_expression (unwrap (If_Expression-gaurd exp)) (unwrap (If_Expression-ifTrue exp)) (unwrap (If_Expression-ifFalse exp)))]
    [(Assignment_Statement? exp) (generate_assignment_statement (generate_expression (unwrap (Assignment_Statement-identifier exp))) (unwrap (Assignment_Statement-exp exp)))]
    [(Enum_Statement? exp) (generate_enum_statement (Variable_Expression-value (unwrap (Enum_Statement-identifier exp))) (unwrap (Enum_Statement-cases exp)))]
    [(Enum_Reference_Statement? exp) (generate_enum_reference_statement (Enum_Reference_Statement-enum_name exp) (Enum_Reference_Statement-enum_case))]
    [(Switch_Statement? exp) (generate_switch_statement (unwrap (Switch_Statement-exp exp)) (unwrap (Switch_Statement-cases exp)) (unwrap (Switch_Statement-default exp)))]
    [(ParseResult? exp) (generate_expression (unwrap exp))]
    [else (error "temporary" exp)]))


(define (collect_switch_cases cases)
  (string-join (list "[" (string-join (map (lambda (arg) (string-join (list (generate_expression (unwrap (first arg))) " : " (generate_expression (unwrap (second arg)))) "")) cases) ", ") "]")))


(define (generate_switch_statement exp cases default)
  (string-join (list "switch(" (generate_expression exp) ", " (collect_switch_cases (append cases (list (list (String_Expression "default") (unwrap (default)))))) ")") ""))


(define (generate_call_expression name arguments retval)
  (if (not (hash-has-key? program name))
      (string-join (list "call_closure(" (generate_expression name) ", " (string-replace (grab_call_arguments arguments (list)) ")" (string-join (list ", " retval "))") ""))) "")
      (string-join (list name "(" (string-replace (grab_call_arguments arguments (list)) ")" ", " retval "))")) "")))

(define (generate_nested_clause name parameters body returned retval)
  (string-join (list (string-upcase name) " = (" (string-replace (grab_call_arguments parameters (list)) ")" (string-join (list ", " retval ")"))) (generate_clause_body body (list)) ", " (generate_assignment_statement retval returned) ")")""))

(define (generate_enum_statement name cases)
  (string-join (map (lambda (arg)  (string-join (list name "(" arg ").") "")) cases) ""))

(define (generate_enum_reference_statement name case)
  (string-join (list name "(" case ")")))


(define (grab_call_arguments arguments collection)
  (if (null? arguments)
      (string-join (list "(" (string-join collection ",") ")") "")
      (grab_call_arguments (rest arguments) (append collection (list (generate_expression (second (first arguments))))))))


(define (generate_clause name parameters body returned retval)
  (if nested
      (generate_nested_clause name parameters body returned generate_Retval)
      (string-join (list name (string-replace (grab_call_arguments parameters (list)) ")" (string-join (list ", " retval ")") "")) " :- " (generate_clause_body body (list)) (string-join (list ", " retval " is " (generate_expression returned))"") ".") "")))

(define (generate_clause_body body collection)
  (if (empty? body)
      (if (empty? collection)
          ""
          (string-join collection ", "))
      (let ([i (ParseResult-result (first body))])
        (if (Function_Expression? i) (set! nested #true) (set! nested #false))
        (generate_clause_body (rest body) (append collection (list (generate_expression i)))))))

(define (generate_if_expression guard ifTrue ifFalse)
  (string-join (list "(" (generate_expression guard) "->" (generate_expression ifTrue) ";" (generate_expression ifFalse) ")") " "))
  
  

(map (lambda (arg) (generate_expression (unwrap arg))) ast_list)