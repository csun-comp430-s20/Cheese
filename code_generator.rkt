#lang racket
(require "parser.rkt")
(require "typechecker.rkt")


(define nested #false)

(define (unwrap exp_or_stmt) (ParseResult-result exp_or_stmt))

(define closure_rule "call_closure(Definition, Params) :- copy_term(Definition, (Params :- Body)), call(Body).")

(define retval_count 0)

(define generate_Retval (let ([i retval_count]) (set! retval_count (add1 retval_count)) (string-join (list "Retval" i) "")))

(define (generate_assignment_statement name exp)
  (cond
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (string-join (list name "is" (generate_additive_or_multiplicative_expression exp) "\n")"")]
    [(Integer_Expression? exp) (string-join (list name "is" (generate_expression exp)))]
    [(Call_Expression? exp) (let ([retval generate_Retval]) (string-join (list (generate_call_expression (unwrap (Call_Expression-identifier exp)) (unwrap (Call_Expression-arguments exp)) retval) ", " name " = " retval) ""))]
    [else (string-join (list name "=" (generate_expression exp)))]))

(define (generate_additive_or_multiplicative_expression exp)
  (if (Additive_Expression? exp)
      (string-join (list (generate_expression (unwrap (Additive_Expression-primary1 exp))) (Additive_Expression-operand exp) (generate_expression (ParseResult-result (Additive_Expression-primary2 exp))))"")
      (string-join (list (generate_expression (unwrap (Multiplicative_Expression-primary1 exp))) (check_for_division (unwrap (Multiplicative_Expression-operand exp))) (generate_expression (unwrap (Multiplicative_Expression-primary2 exp))))"")))


(define (check_for_division operand)
  (if (equal? operand "/") "div" operand))


(define (generate_expression exp)
  (cond
    [(Integer_Expression? exp) (Integer_Expression-value exp)]
    [(String_Expression? exp) (string-join (list #\' (String_Expression-value exp) #\')"")]
    [(Boolean_Expression? exp) (Boolean_Expression-value exp)]
    [(Variable_Expression? exp) (string-upcase (Variable_Expression-value exp))]
    [(Boolean_Operation_Expression? exp) (string-join (list (generate_expression (unwrap (Boolean_Operation_Expression-primary1))) (unwrap (Boolean_Operation_Expression-operand exp)) (generate_expression (unwrap (Boolean_Operation_Expression-primary2 exp))))"")]
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (generate_additive_or_multiplicative_expression exp)]
    [(Call_Expression? exp) (generate_call_expression (unwrap (Call_Expression-identifier exp)) (unwrap (Call_Expression-arguments exp)) generate_Retval)]
    [(Function_Expression? exp) (generate_clause (unwrap (Function_Expression-identifier exp)) (unwrap (Function_Expression-parameters exp)) (unwrap (Function_Expression-body exp)) (unwrap (Function_Expression-returned exp)))]
    [(If_Expression? exp) (generate_if_expression (unwrap (If_Expression-gaurd exp)) (unwrap (If_Expression-ifTrue exp)) (unwrap (If_Expression-ifFalse exp)))]
    [(Assignment_Statement? exp) (generate_assignment_statement (generate_expression (unwrap (Assignment_Statement-identifier exp))) (unwrap (Assignment_Statement-exp exp)))]
    [(Enum_Statement? exp) (generate_enum_statement (Variable_Expression-value (ParseResult-result (Enum_Statement-identifier exp))) (ParseResult-result (Enum_Statement-cases exp)))]
    [(Enum_Reference_Statement? exp) (generate_enum_reference_statement (Enum_Reference_Statement-enum_name exp) (Enum_Reference_Statement-enum_case))]
    [(ParseResult? exp) (generate_expression (ParseResult-result exp))]
    [else (error "temporary" exp)]))


(define (generate_call_expression name arguments retval)
  (if (not (hash-has-key? name))
      (string-join (list "call_closure(" (generate_expression name) ", " (string-replace (grab_call_arguments arguments (list)) ")" ", " retval "))")) "")
      (string-join (list name "(" (string-replace (grab_call_arguments arguments (list)) ")" ", " retval "))")) "")))

(define (generate_nested_clause name parameters body returned retval)
  (string-join (list (generate_expression name) " = (" (string-replace (grab_call_arguments parameters (list)) ")" (string-join (list ", " retval ")"))) (generate_clause_body body (list)) ", " (generate_assignment_statement retval returned) ")")"")
  (set! nested #false))

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
      (generate_nested_clause name parameters body returned)
      (string-append* (list name (string-replace (grab_call_arguments parameters (list)) ")"(string-join (list ", " retval ")"))) " :- " (generate_clause_body body (list)) (string-join (list ", " retval "is " (generate_expression returned))"") "."))))

(define (generate_clause_body body collection)
  (if (null? body)
      (string-join collection ", ")
      (let ([i (ParseResult-result (first body))])
        (if (Function_Expression? i) (set! nested #true) (set! nested #false))
        (generate_clause_body (rest body) (append collection (list (generate_expression i)))))))

(define (generate_if_expression guard ifTrue ifFalse)
  (string-join (list "(" (generate_expression guard) "->" (generate_expression ifTrue) ";" (generate_expression ifFalse) ")") " "))
  
  
  (generate_expression (ParseResult-result (first ast_list)))