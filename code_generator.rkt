#lang racket
(require "parser.rkt")
(require "typechecker.rkt")

(define (generate_assignment_statement name exp)
  (cond
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (string-join (list name "is" (generate_additive_or_multiplicative_expression exp) "\n")"")]
    [(Integer_Expression? exp) (string-join (list name "is" (generate_expression exp)))]
    [else (string-join (list name "=" (generate_expression exp)))]))

(define (generate_additive_or_multiplicative_expression exp)
  (if (Additive_Expression? exp)
      (string-join (list (generate_expression (ParseResult-result (Additive_Expression-primary1 exp))) (Additive_Expression-operand exp) (generate_expression (ParseResult-result (Additive_Expression-primary2 exp))))"")
      (string-join (list (generate_expression (ParseResult-result (Multiplicative_Expression-primary1 exp))) (check_for_division (ParseResult-result (Multiplicative_Expression-operand exp))) (generate_expression (ParseResult-result (Multiplicative_Expression-primary2 exp))))"")))


(define (check_for_division operand)
  (if (equal? operand "/") "div" operand))


(define (generate_expression exp)
  (cond
    [(Integer_Expression? exp) (Integer_Expression-value exp)]
    [(String_Expression? exp) (string-join (list #\' (String_Expression-value exp) #\')"")]
    [(Boolean_Expression? exp) (Boolean_Expression-value exp)]
    [(Variable_Expression? exp) (string-upcase (Variable_Expression-value exp))]
    [(Boolean_Operation_Expression? exp) (string-join '((generate_expression (ParseResult-result (Boolean_Operation_Expression-primary1))) (ParseResult-result (Boolean_Operation_Expression-operand exp)) (generate_expression (ParseResult-result (Boolean_Operation_expression-primary2 exp))))"")]
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (generate_additive_or_multiplicative_expression exp)]
    [(Call_Expression? exp) (string-join '((generate_expression (ParseResult-result (Call_Expression-identifier exp))) (grab_call_arguments (ParseResult-result (Call_Expression-arguments exp)) (list))""))]
    [(Function_Expression? exp) (generate_clause (ParseResult-result (Function_Expression-identifier exp)) (ParseResult-result (Function_Expression-parameters exp)) (ParseResult-result (Function_Expression-body exp)) (ParseResult-result (Function_Expression-returned exp)))]
    [(If_Expression? exp) (generate_if_expression (ParseResult-result (If_Expression-gaurd exp)) (ParseResult-result (If_Expression-ifTrue exp)) (ParseResult-result (If_Expression-ifFalse exp)))]
    [(Assignment_Statement? exp) (generate_assignment_statement (ParseResult-result (Assignment_Statement-identifier exp)) (ParseResult-result (Assignment_Statement-exp exp)))]
    [(Enum_Statement? exp) (generate_enum_statement (Variable_Expression-value (ParseResult-result (Enum_Statement-identifier exp))) (ParseResult-result (Enum_Statement-cases exp)))]
    [(Enum_Reference_Statement? exp) (generate_enum_reference_statement (Enum_Reference_Statement-enum_name exp) (Enum_Reference_Statement-enum_case))]
    [(ParseResult? exp) (generate_expression (ParseResult-result exp))]
    [else (error "temporary" exp)]))


(define (generate_enum_statement name cases)
  (string-join (map (lambda (arg)  (string-join (list name "(" arg ").") ""))) ""))

(define (generate_enum_reference_statement name case)
  (string-join (list name "(" case ")")))


(define (grab_call_arguments arguments collection)
  (if (null? arguments)
      (string-join (list "(" (string-join collection ",") ")") "")
      (grab_call_arguments (rest arguments) (append collection (list (generate_expression (second (first arguments))))))))


(define (generate_clause name parameters body returned)
  (string-append* (list name (string-replace (grab_call_arguments parameters (list)) ")" ", Retval)") " :- " (generate_clause_body body (list)) ", Retval is " (generate_expression returned) ".")))

(define (generate_clause_body body collection)
  (if (null? body)
      (string-join collection ", ")
      (generate_clause_body (rest body) (append collection (list (generate_expression (ParseResult-result (first body))))))))

(define (generate_if_expression guard ifTrue ifFalse)
  (string-join (list "(" (generate_expression guard) "->" (generate_expression ifTrue) ";" (generate_expression ifFalse) ")") " "))
  
  
  (generate_expression (ParseResult-result (first ast_list)))