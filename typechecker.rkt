#lang racket
(require "parser.rkt")

(struct Int_Type ([hash_code #:auto])
  #:auto-value 0)

(struct String_Type ([hash_code #:auto])
  #:auto-value 1)

(struct Bool_Type ([hash_code #:auto])
  #:auto-value 2)

(define gamma (make-hash))

(define (type_of gamma exp)
  (cond
    [(Integer_Expression? exp) (Int_Type)]
    [(String_Expression? exp) (String_Type)]
    [(Boolean_Expression? exp) (Bool_Type)]
    [(Variable_Expression? exp) (if (hash-has-key? gamma (Variable_Expression-value exp)) (hash-ref (Variable_Expression-value exp)) (error "Variable " (Variable_Expression-value exp) " is out of scope."))]
    [(Additive_Expression? exp) (if (and (Int_Type? (type_of gamma (ParseResult-result (Additive_Expression-primary1 exp)))) (Int_Type? (type_of gamma (ParseResult-result (Additive_Expression-primary2 exp))))) (Int_Type) (error "one or more expressions is not an int in an additive expression"))]
    [(Multiplicative_Expression? exp) (if (and (Int_Type? (type_of gamma (ParseResult-result (Multiplicative_Expression-primary1 exp)))) (Int_Type? (type_of gamma (ParseResult-result (Multiplicative_Expression-primary2 exp))))) (Int_Type) (error "one or more expressions is not an int in a multiplicative expression"))]
    [(Boolean_Operation_Expression? exp)
     (let ([exp1 (type_of gamma (ParseResult-result (Boolean_Operation_Expression-primary1 exp)))] [exp2 (type_of gamma (ParseResult-result (Boolean_Operation_Expression-primary2 exp)))])
       (cond
         [(and (Bool_Type? exp1) (Bool_Type? exp2)) (Bool_Type)]
         [(and (Int_Type? exp1) (Int_Type? exp2)) (Bool_Type)]
         [else (error "one or more expressions is not a boolean in a boolean operation")]))]
    [(If_Expression? exp)
     (let ([gaurd (type_of gamma (ParseResult-result (If_Expression-gaurd exp)))] [ifTrue (ParseResult-result (If_Expression-ifTrue exp))] [ifFalse (ParseResult-result (If_Expression-ifFalse exp))])
       (if (Bool_Type? gaurd) (check_ifTrue_and_ifFalse gamma ifTrue ifFalse gaurd) (error "gaurd for if expression is type: " gaurd ". Expected type boolean")))]
    [(Assignment_Statement? exp)
     (let ([tau (determine_type_of (ParseResult-result (Assignment_Statement-type exp)))] [name (ParseResult-result (Assignment_Statement-identifier exp))] [e (type_of gamma (ParseResult-result (Assignment_Statement-exp exp)))])
       (if (equal? (object-name tau) (object-name e)) (update_gamma_and_return_tau gamma name tau) (error "Type " tau " cannot be converted to " e)))]
    [(While_Statement? exp)
     (let ([gaurd (type_of gamma (ParseResult-result (While_Statement-gaurd exp)))] [body (ParseResult-result (While_Statement-body exp))])
       (if (Bool_Type? gaurd) (type_of (hash-copy gamma) body) (error "While statement expected a gaurd of type boolean but was given a gaurd of type: " gaurd)))]
    [(Function_Expression? exp)
     (let ([type (determine_type_of (ParseResult-result (Function_Expression-type exp)))] [name (ParseResult-result (Function_Expression-identifier exp))])
       (if (not (hash-has-key? gamma name)) (type_check_function gamma type name (ParseResult-result (Function_Expression-parameters)) (ParseResult-result (Function_Expression-body)) (ParseResult-result (Function_Expression-returned))) (error name " has already been defined")))]
    [(Call_Expression? exp) ;in progress
    [else (error "unrecognized expression")]))


(define (type_check_function gamma type name parameters body returned)
  (hash-set! gamma (name (list type (collect_function_parameters_types (list) parameters))))
  (let ([copy (hash-copy gamma)])
    (update_gamma_with_function_parameters copy parameters)
    (type_of copy body)
    (if (equal? (object-name type) (object-name (type_of copy returned))) type (error name " does not return a value of type: " type))))

(define (collect_function_parameters_types param_types parameters)
  (if (not (null? parameters))
      (collect_function_parameters (append param_types (list (determine_type_of (ParseResult-result (first (first parameters)))))) (rest parameters))
      param_types))


(define (update_gamma_with_function_parameters copy parameters)
  (if (not (null? parameters))
      (update_gamma_with_function_parameters (hash-set! copy ((ParseResult-result (second (first parameters))) (determine_type_of (ParseResult-result (first (first parameters)))))) (rest parameters))
      copy))

(define (check_ifTrue_and_ifFalse gamma ifTrue ifFalse gaurd)
  (type_of (hash-copy gamma) ifTrue)
  (type_of (hash-copy gamma) ifFalse)
  gaurd)


 

(define (update_gamma_and_return_tau gamma name tau) (hash-set! gamma (name tau)) tau)


(define (determine_type_of tau)
  (cond
    [(equal? tau "int") (Int_Type)]
    [(equal? tau "String") (String_Type)]
    [(equal? tau "boolean") (Bool_Type)]
    [else (error "unrecognized type: " tau)]))