#lang racket
;begin parsing tokens
(require "tokenizer.rkt")
;amount of tokens to parse
(define amount_of_tokens (length Tokens))

;structs for the expressions and statements in our language
(struct If_Expression (gaurd ifTrue ifFalse))
(provide (struct-out If_Expression))
(struct Additive_Expression (operand primary1 primary2))
(provide (struct-out Additive_Expression))
(struct Multiplicative_Expression (operand primary1 primary2))
(provide (struct-out Multiplicative_Expression))
(struct Boolean_Operation_Expression (operand primary1 primary2))
(provide (struct-out Boolean_Operation_Expression))
(struct While_Statement (gaurd body))
(provide (struct-out While_Statement))
(struct Enum_Statement (identifier cases))
(provide (struct-out Enum_Statement))
(struct Or_Statement (exp1 exp2))
(provide (struct-out Or_Statement))
(struct Fail_Statement (default))
(provide (struct-out Fail_Statement))
(struct Switch_Statement (exp cases default))
(provide (struct-out Switch_Statement))
(struct Call_Expression (identifier arguments))
(provide (struct-out Call_Expression))
(struct Assignment_Statement (type identifier exp))
(provide (struct-out Assignment_Statement))
(struct Print_Statement (exp))
(provide (struct-out Print_Statement))
(struct Integer_Expression (value))
(provide (struct-out Integer_Expression))
(struct String_Expression (value))
(provide (struct-out String_Expression))
(struct Boolean_Expression (value))
(provide (struct-out Boolean_Expression))
(struct Variable_Expression (value))
(provide (struct-out Variable_Expression))
(struct Function_Expression (type identifier parameters body returned))
(provide (struct-out Function_Expression))
(struct Enum_Reference_Statement (enum_name enum_case))
(provide (struct-out Enum_Reference_Statement))

;struct used to encapsulate expressions and statements and point to the next token (how we're building the ast)
(struct ParseResult (result nextpos))
(provide (struct-out ParseResult))

(define (get_token pos) (list-ref Tokens pos))

(define (is_leftparen pos) (leftparen_Token? (get_token pos)))
(define (is_rightparen pos) (rightparen_Token? (get_token pos)))

;helper function, checks if the token at position pos and position pos + 1 are tokens that prefix a function decleration
(define (is_function_expression pos) (and (is_leftparen pos) (function_Token? (get_token (add1 pos)))))

;attempts to parse a function
(define (Parse_function pos)
  ;check if there are tokens left
  (if (< pos amount_of_tokens)
      ;check for function decleration prefix: (def 
      (if (is_function_expression pos)
          (let* (
                 ;t is the type of the function: int, String, boolean
                 [t (collect_variable_type (+ pos 2))]
                 ;i is the name of the function, an identifier
                 [i (collect_variable_name (ParseResult-nextpos t))]
                 ;p are the parameters
                 [p (parse_parameter_decleration (ParseResult-nextpos i))]
                 ;b is the body
                 [b (parse_function_body_decleration (ParseResult-nextpos p))]
                 ;r is the expression being returned
                 [r (Parse_Expression (ParseResult-nextpos b))])
            ;check to see if we've reached the end of the function, i.e. is the token at position pos a rightparen_Token
            (if (is_rightparen (ParseResult-nextpos r))
                ;if so retrun a ParseResult containing the Function expression and the next position (pos + 1)
                (ParseResult(Function_Expression t i p b r) (add1 (ParseResult-nextpos r)))
                ;else throw an error due to invalid syntax
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos r)))))
          ;if not a function then try parsing a stmt
          (Parse_Statement pos))
      ;temporary... don't know what else to return if we run out of tokens
      pos))

;prepare to parse function parameters
(define (parse_parameter_decleration pos)
  (if (< pos amount_of_tokens)
      (if (is_leftparen pos)
          (collect_params (add1 pos) (list))
          (error "invalid syntax, expected ) but read : " (get_token pos)))
      (error "ran out of tokens while parsing")))

;collects the function parameters
(define (collect_params pos params)
  ;are we done parsing parameters? i.e. has a rightpaen_Token been read if so return a ParseResult with a list of parameters and next position
  (if (is_rightparen pos) (ParseResult params (add1 pos))
      ;t is the type of parameter
      (let* ([t (check_type_of_param (get_token pos) pos)]
             ;i is the variable name
            [i (check_name (get_token (ParseResult-nextpos t)) (ParseResult-nextpos t))])
        ;recusively call until a rightparen_Token has been read
        (collect_params (ParseResult-nextpos i)(append params (list (list t i)))))))

;verifies the token at position pos is a type_Token and returns a ParseResult containing the type and next position else an error is throw
(define (check_type_of_param tok pos)
  (if (< pos amount_of_tokens)
      (if (type_Token? tok)
          (ParseResult (type_Token-value tok) (add1 pos))
          (error "invalid type given, expected a type int or string or boolean but read: " (get_token pos)))
      (error "ran out of tokens while parsing")))

;verifies the token at position pos is an identifier_Token and returns a ParseResult containing a Variable_Expression and the next position
(define (check_name tok pos)
  (if (< pos amount_of_tokens)
      (if (identifier_Token? tok)
          (ParseResult (Variable_Expression (identifier_Token-value tok)) (add1 pos))
          (error "expected a variable, read: " (get_token pos)))
      (error "ran out of tokens while parsing")))

;prepares to parse a function body
(define (parse_function_body_decleration pos)
  (if (< pos amount_of_tokens)
      (if (leftcurly_Token? (get_token pos))
          (collect_body (add1 pos) (list))
          (error "invalid syntax, expected: } but read " (get_token pos)))
      (error "ran out of tokens while parsing")))

;collects the function body
(define (collect_body pos body)
  (if (< pos amount_of_tokens)
      ;are we done parsing the body? i.e. has a rightculry_Token been read?
      (if (rightcurly_Token? (get_token pos))
          ;if so return a ParseResult containing the body (a list of expressions or statements) and the next position
          (ParseResult body (add1 pos))
          ;else continue parsing the body by recursively calling collect_body
          (let ([exp_or_stmt (Parse_function pos)])
            (collect_body (ParseResult-nextpos exp_or_stmt) (append body (list exp_or_stmt)))))
      (error "ran out of tokens while parsing")))

; check if starts with leftParen and While Token "(While"
(define (isWhile pos)
  (and (is_leftparen pos)
       (while_Token? (get_token (add1 pos)))))

; collect everything until the end of condition ")"
(define (collect_condition pos condition)
  (if (is_rightparen pos)
      (ParseResult condition (add1 pos))
      (let ([cond_stmt (Parse_Expression pos)])
        (collect_condition (ParseResult-nextpos cond_stmt) (append condition (list cond_stmt))))   
  ))

; parse the gaurd a.k.a. the condition of while loop.
; Check for "(" then collect the condition. 
(define (parse_gaurd pos)
  (if (is_leftparen pos)
      (collect_condition (add1 pos) (list))
      (error "invalid syntax, expected ( but read : " (get_token pos))
      )
  )

; parse the body of while loop. Check for "(" then collect body
 (define (parse_body pos)
  (if (leftcurly_Token? (get_token pos))
      (collect_while_body (add1 pos) (list))
      (error "invalid syntax, expected ( but read : " (get_token pos))
      )
  )

; collect everything in body until end of body ")"
(define (collect_while_body pos body)
  (if (rightcurly_Token? (get_token pos))
      (ParseResult body (add1 pos))
      (let ([while_body (Parse_Statement pos)])
        (collect_while_body (ParseResult-nextpos while_body) (append body (list while_body)))
        )))
 
; Parse the entire While loop
(define (Parse_Statement pos)
  (if (< pos amount_of_tokens)
      (if (isWhile pos)
          ; g is gaurd (or the condition for keeping the while loop running)
          (let* ([g (parse_gaurd (+ pos 2))]
                 ;b is body
                 [b (parse_body (ParseResult-nextpos g))])
            ; check if While Loop has ended
            (if (is_rightparen (ParseResult-nextpos b))
                ;if so retrun a ParseResult containing the while expression and the next position (pos + 1)
                (ParseResult (While_Statement g b) (add1 (ParseResult-nextpos b)))
                ;else throw an error due to invalid syntax
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos b)
                 ))))
          ; if not a While stmt then try to parse an enum stmt
          (Parse_Enumerate_Statement pos)
          )
      ; ran out of tokens
      (ParseResult null pos)
  ))

(define (Parse_Enumerate_Statement pos)
  (if (< pos amount_of_tokens)
      (if (an_enum_stmt pos)
          (let* ([identifier (collect_enum_name (+ pos 2))]
                 [cases (collect_enum_cases (ParseResult-nextpos identifier))])
            (if (is_rightparen (ParseResult-nextpos cases))
                (ParseResult (Enum_Statement identifier cases) (add1 (ParseResult-nextpos cases)))
                (error "incorrect syntax, missing a )")))
          (Parse_Switch_Statement pos))
      (ParseResult null pos)))

(define (an_enum_stmt pos)
  (if (< pos amount_of_tokens)
      (and (is_leftparen pos) (enum_Token? (get_token (add1 pos))))
      (error "ran out of tokens while parsing")))

(define (collect_enum_name pos)
  (if (< pos amount_of_tokens)
      (if (identifier_Token? (get_token pos))
          (ParseResult (Variable_Expression (identifier_Token-value (get_token pos))) (add1 pos))
          (error "invalid syntax, expected a variable but read: " (get_token pos)))
      (error "ran out of tokens while parsing")))

(define (collect_enum_cases pos)
  (if (< pos amount_of_tokens)
      (if (is_leftparen pos)
          (retrieve_enum_cases (add1 pos) (list))
          (error "invalid syntax, expected: ( but read: " (get_token pos)))
      (error "ran out of tokens while parsing")))

(define (retrieve_enum_cases pos cases)
  (if (< pos amount_of_tokens)
      (if (is_rightparen pos)
          (ParseResult cases (add1 pos))
          (let ([a_case (get_case pos)])
            (retrieve_enum_cases (ParseResult-nextpos a_case) (append cases (list a_case)))))
      (error "ran out of tokens while parsing")))


(define (get_case pos)
  (if (< pos amount_of_tokens)
      (if (and (case_Token? (get_token pos)) (identifier_Token? (get_token (add1 pos))))
          (ParseResult (Variable_Expression (identifier_Token-value (get_token (add1 pos)))) (+ pos 2))
          (error "invalid syntax, expected: case but read: " (get_token pos)))
      (error "ran out of tokens while parsing")))


(define (Parse_Switch_Statement pos)
  (if (< pos amount_of_tokens)
      (if (a_switch_stmt pos)
          (let* ([exp (Parse_Enum_Reference_Statement (+ pos 2))]
                 [cases (collect_switch_cases (ParseResult-nextpos exp))]
                 [default (collect_default_case (ParseResult-nextpos cases))])
            (if (is_rightparen (ParseResult-nextpos default))
                (ParseResult (Switch_Statement exp cases default) (add1 (ParseResult-nextpos default)))
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos default)))))
          (Parse_Or pos))
      (ParseResult null pos)))

(define (a_switch_stmt pos)
  (if (< pos amount_of_tokens)
      (and (is_leftparen pos) (switch_Token? (get_token (add1 pos))))
      (error "ran out of tokens while parsing")))

(define (collect_switch_cases pos)
  (if (< pos amount_of_tokens)
      (retrieve_switch_cases pos (list))
      (error "ran out of tokens while parsing")))

(define (retrieve_switch_cases pos cases)
  (if (< pos amount_of_tokens)
      (if (and (is_leftparen pos) (case_Token? (get_token (add1 pos))))
          (let* ([exp1 (Parse_Enum_Reference_Statement (+ pos 2))]
                 [exp2 (Parse_Expression (ParseResult-nextpos exp1))])
            (if (is_rightparen (ParseResult-nextpos exp2))
                (retrieve_switch_cases (add1 (ParseResult-nextpos exp2)) (append cases (list (list exp1 exp2))))
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos exp2)))))
          (ParseResult cases pos))
      (error "ran out of tokens while parsing")))

(define (collect_default_case pos)
  (if (< pos amount_of_tokens)
      (if (and (is_leftparen pos) (default_Token? (get_token (add1 pos))))
          (let ([exp (Parse_Expression (+ pos 2))])
            (if (is_rightparen (ParseResult-nextpos exp))
                (ParseResult exp (add1 (ParseResult-nextpos exp)))
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos exp)))))
          (error "invalid syntax, missing default case" (get_token (add1 pos))))
      (error "ran out of tokens while parsing")))



; Check if stmt starts with leftparen and Or token: "(or"
(define (isOr pos)
  (and (is_leftparen pos)
       (or_Token? (get_token (add1 pos)))))

; Parse the Or Stmt
(define (Parse_Or pos)
  (if (< pos amount_of_tokens)
      (if (isOr pos)
          ; e1 is expression 1
          (let* ([e1 (Parse_Expression (+ pos 2))]
                 [e2 (Parse_Expression (ParseResult-nextpos e1))])
            ; if end of Or Stmt
            (if (is_rightparen (ParseResult-nextpos e2))
                ;if so retrun a ParseResult containing the while expression and the next position (pos + 1)
                (ParseResult (Or_Statement e1 e2) (add1 (ParseResult-nextpos e2)))
                ;else throw an error due to invalid syntax
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)
                 ))))
          ; not an Or stmt
          (Parse_Assignment_Statement pos)
          )
      ; ran out of tokens
      (ParseResult null pos)
      )
  )

(define (Parse_Assignment_Statement pos)
  (if (< pos amount_of_tokens)
      (if (an_assignment pos)
          (let* ([type (collect_variable_type (+ pos 2))]
                 [name (collect_variable_name (ParseResult-nextpos type))]
                 [exp (Parse_Expression (ParseResult-nextpos name))])
            (if (is_rightparen (ParseResult-nextpos exp))
                (ParseResult (Assignment_Statement type name exp) (add1 (ParseResult-nextpos exp)))
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos exp)))))
          (Parse_Fail_Statement pos))
      (ParseResult null pos)))

(define (an_assignment pos)
  (if (< pos amount_of_tokens)
      (if (and (is_leftparen pos) (operator_Token? (get_token (add1 pos))))
          (equal? "=" (operator_Token-value (get_token (add1 pos))))
          #f)
      (error "ran out of tokens while parsing")))

(define (collect_variable_type pos)
  (if (< pos amount_of_tokens)
      (if (type_Token? (get_token pos))
          (ParseResult (type_Token-value (get_token pos)) (add1 pos))
          (error "invalid syntax, missing variable type"))
      (error "ran out of tokens while parsing")))

(define (collect_variable_name pos)
  (if (< pos amount_of_tokens)
      (if (identifier_Token? (get_token pos))
          (ParseResult (identifier_Token-value (get_token pos)) (add1 pos))
          (error "invalid syntax, missing variable name"))
      (error "ran out of tokens while parsing")))

(define (Parse_Fail_Statement pos)
  (if (< pos amount_of_tokens)
      (if (fail_Token? (get_token pos)) (ParseResult (Fail_Statement (fail_Token-default (get_token pos))) (add1 pos))(Parse_Print_Statement pos))
      (ParseResult null pos)))

(define (Parse_Print_Statement pos)
  (if (< pos amount_of_tokens)
      (if (and (leftparen_Token? (get_token pos)) (print_Token? (get_token (add1 pos))))
          (let ([exp (Parse_Primary (+ pos 2))])
            (if (is_rightparen (ParseResult-nextpos exp))
                (ParseResult (Print_Statement exp) (add1 (ParseResult-nextpos exp)))
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos exp)))))
          (Parse_Enum_Reference_Statement pos))
      (ParseResult null pos)))

(define (Parse_Enum_Reference_Statement pos)
  (if (< pos amount_of_tokens)
      (if (identifier_Token? (get_token pos))
          (if (period_Token? (get_token (add1 pos)))
              (if (identifier_Token? (get_token (+ pos 2)))
                  (ParseResult (Enum_Reference_Statement (identifier_Token-value (get_token pos)) (identifier_Token-value (get_token (+ pos 2)))) (+ pos 3))
                  (error "expected an enum case but read " (get_token (+ pos 2))))
              (Parse_Expression pos))
          (Parse_Expression pos))
      (ParseResult null pos)))

(define (Parse_Expression pos)
  (if (< pos amount_of_tokens)
      (if (an_if pos)
          (let* ([gaurd (Parse_Expression (+ pos 2))]
                 [ifTrue (Parse_Expression (ParseResult-nextpos gaurd))]
                 [ifFalse (collect_ifFalse_body (ParseResult-nextpos ifTrue))])
            (if (is_rightparen (ParseResult-nextpos ifFalse))
                (ParseResult (If_Expression gaurd ifTrue ifFalse) (add1 (ParseResult-nextpos ifFalse)))
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos ifFalse)))))
          (Parse_Additive_Expression pos))
      (ParseResult null pos)))

(define (an_if pos) (and (is_leftparen pos) (if_Token? (get_token (add1 pos)))))

(define (collect_ifFalse_body pos)
  (if (< pos amount_of_tokens)
      (if (else_Token? (get_token pos))
          (let ([exp (Parse_Expression (add1 pos))])
            (if (is_rightparen (ParseResult-nextpos exp))
                exp
                (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos exp)))))
          (error "invalid syntax, expected: else but read: " (get_token pos)))
      (ParseResult null pos)))

(define (Parse_Additive_Expression pos)
  (if (< pos amount_of_tokens)
      (cond
        [(and (is_leftparen pos) (a_plus_operator (add1 pos))) (assemble_addition_expression (+ pos 2))]
        [(and (is_leftparen pos) (a_subtraction_operator (add1 pos))) (assemble_subtraction_expression (+ pos 2))]
        [else (Parse_Multiplicative_Expression pos)])
      (ParseResult null pos)))

(define (a_plus_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? "+" (operator_Token-value op))
            #f))
      (error "ran out of tokens while parsing")))

(define (a_subtraction_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? "-" (operator_Token-value op))
            #f))
      (error "ran out of tokens while parsing")))

(define (assemble_addition_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Additive_Expression "+" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (assemble_subtraction_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Additive_Expression "-" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (Parse_Multiplicative_Expression pos)
  (if (< pos amount_of_tokens)
      (cond
        [(and (is_leftparen pos) (a_multiplication_operator (add1 pos))) (assemble_multiplication_expression (+ pos 2))]
        [(and (is_leftparen pos) (a_division_operator (add1 pos))) (assemble_division_expression (+ pos 2))]
        [else (Parse_Boolean_Operation_Expression pos)])
      (ParseResult null pos)))

(define (a_multiplication_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? "*" (operator_Token-value op))
            #f))
      (error "ran out of tokens while parsing")))

(define (a_division_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? "/" (operator_Token-value op))
            #f))
      (error "ran out of tokens while parsing")))

(define (assemble_multiplication_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Multiplicative_Expression "*" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (assemble_division_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Multiplicative_Expression "/" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (Parse_Boolean_Operation_Expression pos)
  (if (< pos amount_of_tokens)
      (cond
        [(and (is_leftparen pos) (an_equality_operator (add1 pos))) (assemble_eqaulity_expression (+ pos 2))]
        [(and (is_leftparen pos) (a_lessthanequal_operator (add1 pos))) (assemble_lessthanequal_expression (+ pos 2))]
        [(and (is_leftparen pos) (a_greaterthanequal_operator (add1 pos))) (assemble_greaterthanequal_expression (+ pos 2))]
        [(and (is_leftparen pos) (a_lessthan_operator (add1 pos))) (assemble_lessthan_expression (+ pos 2))]
        [(and (is_leftparen pos) (a_greaterthan_operator (add1 pos))) (assemble_greaterthan_expression (+ pos 2))]
        [(and (is_leftparen pos) (a_logic_and_operator (add1 pos))) (assemble_logic_and_expression (+ pos 2))]
        [else (Parse_function_call_expression pos)])
      (ParseResult null pos)))

(define (an_equality_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? "==" (operator_Token-value op)) 
            #f))
      (error "ran out of tokens while parsing")))

(define (a_lessthanequal_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? "<=" (operator_Token-value op))
            #f))
      (error "ran out of tokens to parse")))

(define (a_greaterthanequal_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? ">=" (operator_Token-value op))
            #f))
      (error "ran out of tokens to parse")))

(define (a_lessthan_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? "<" (operator_Token-value op))
            #f))
      (error "ran out of tokens to parse")))

(define (a_greaterthan_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (if (operator_Token? op)
            (equal? ">" (operator_Token-value op))
            #f))
      (error "ran out of tokens to parse")))

(define (a_logic_and_operator pos)
  (if (< pos amount_of_tokens)
      (let ([op (get_token pos)])
        (and_Token? op))
      (error "ran out of tokens to parse")))

(define (assemble_lessthanequal_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Boolean_Operation_Expression "<=" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (assemble_greaterthanequal_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Boolean_Operation_Expression ">=" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (assemble_lessthan_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Boolean_Operation_Expression "<" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (assemble_greaterthan_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Boolean_Operation_Expression ">" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (assemble_eqaulity_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Boolean_Operation_Expression "==" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

(define (assemble_logic_and_expression pos)
  (if (< pos amount_of_tokens)
      (let* ([e1 (Parse_Primary pos)]
             [e2 (Parse_Primary (ParseResult-nextpos e1))])
        (if (is_rightparen (ParseResult-nextpos e2))
            (ParseResult (Boolean_Operation_Expression "and" e1 e2) (add1 (ParseResult-nextpos e2)))
            (error "invalid syntax, expected: ) but read: " (get_token (ParseResult-nextpos e2)))))
      (ParseResult null pos)))

;attempt to parse a function call
(define (Parse_function_call_expression pos)
  ;check if there are still tokens left
  (if (< pos amount_of_tokens)
      ;if the token at position pos and the token at pos + 1 are a leftparen_Token and an identifier_Token
      (if (and (is_leftparen pos) (identifier_Token? (get_token (add1 pos))))
          ;then collect the name of the function being called and the arguments provided
          ;i is the name of the function being called
          (let* ([i (collect_variable_name (add1 pos))]
                 ;a are the arguments being passed in 
                 [a (collect_arguments (ParseResult-nextpos i))])
            ;and return a ParseResult of the Call_Expression and next position
            (ParseResult (Call_Expression i a) (ParseResult-nextpos a)))
          (Parse_Primary pos))
      (ParseResult null pos)))

;prepare to collect the arguments
(define (collect_arguments pos)
  (if (< pos amount_of_tokens)
      (collecting_arguments pos (list))
      (error "was in the middle of parsing but ran out of tokens")))

;collect arguments
(define (collecting_arguments pos args)
  ;check if there are tokens left to parse
  (if (< pos amount_of_tokens)
      ;are we done parsing the arguments i.e. has a rightparen_Token been read
      (if (is_rightparen pos)
          ;if so return a ParseResult of a list of arguments and the next position
          (ParseResult args (add1 pos))
          ;else parse the next argument (an expression) and recursively call collect_arguments until a rightparen_Token has been read
          (let* ([exp (Parse_Expression pos)])
            (collecting_arguments (ParseResult-nextpos exp) (append args (list (list exp))))))
      (error "was in the middle of parsing but ran out of tokens")))

(define (Parse_Primary pos)
  (if (< pos amount_of_tokens)
      (let ([tok (get_token pos)])
        (cond
          [(integer_Token? tok) (ParseResult (Integer_Expression (integer_Token-value tok)) (add1 pos))]
          [(string_Token? tok) (ParseResult (String_Expression (string_Token-value tok)) (add1 pos))]
          [(boolean_Token? tok) (ParseResult (Boolean_Expression (boolean_Token-value tok)) (add1 pos))]
          [(identifier_Token? tok) (ParseResult (Variable_Expression (identifier_Token-value tok)) (add1 pos))]
          [(leftparen_Token? tok)
           (let ([exp (Parse_Expression (add1 pos))])
             (if (is_rightparen (ParseResult-nextpos exp))
                 (ParseResult (ParseResult-result exp) (add1 (ParseResult-nextpos exp)))
                 (error "invalid syntax, expecting: ) but read: " (get_token (ParseResult-nextpos exp)))))]
          [else (error "expected an expression but read: " tok)]))
      (ParseResult null pos)))

(define (toplevelparse pos results)
  (if (< pos amount_of_tokens)
      (let ([result (Parse_function pos)])
        (toplevelparse (ParseResult-nextpos result) (append results (list result))))
      results))


(define ast_list (toplevelparse 0 (list)))
(provide ast_list)
