#lang racket

(define amount_of_tokens (length Tokens))
(define (chop_tokens pos) (take-right Tokens (- amount_of_tokens pos)))

(struct If_expression (gaurd ifTrue ifFalse))
(struct Additive_expression (operand primary1 primary2))
(struct Multiplicative_expression (operand primary1 primary2))
(struct Boolean_expression (operand primary1 primary2))
(struct While_statement (gaurd body))
(struct Enum_statement (identifier cases))
(struct Or_statement (exp1 exp2))
(struct Switch_statement (exp cases default))
(struct Call_statement (type identifier arguments))
(struct Assignment_statement (identifier exp))
(struct Integer_Expression (value))
(struct String_Expression (value))
(struct Boolean_Expression (value))
(struct Variable_Expression (value))
(struct Function_Expression (type identifier parameters body returned))

(struct ParseResult (result nextpos))

(define (is_function_expression pos) (and (leftparen_Token? (list-ref Tokens pos)) (function_Token? (list-ref Tokens (add1 pos)))))

(define (Parse_function pos)
  (if (< pos amount_of_tokens)
      (if (is_function_expression pos)
          (let* ([t (ParseResult (list-ref Tokens (+ pos 2)) (+ pos 3))]
            [i (ParseResult (list-ref Tokens (ParseResult-nextpos t)) (add1 (ParseResult-nextpos t)))]
            [p (parse_parameter_decleration (ParseResult-nextpos i))]
            [b (parse_function_body_decleration (ParseResult-nextpos p))]
            [r (Parse_function (ParseResult-nextpos b))])
            (if (rightparen_Token? (list-ref Tokens (- (ParseResult-nextpos r) 1)))
                (ParseResult (Function_Expression t i p b r) (add1 (ParseResult-nextpos r)))
                (error "invalid syntax, expected: ) but read: " (list-ref Tokens (ParseResult-nextpos r)))))
            ;(Parse_Expression pos))
          (ParseResult null (add1 pos)))
      pos))

(define (parse_parameter_decleration pos)
  (if (leftparen_Token? (list-ref Tokens pos))
      (collect_params (add1 pos) (list))
      (error "invalid syntax, expected ) but read : " (list-ref Tokens pos))))

(define (collect_params pos params)
  (if (rightparen_Token? (list-ref Tokens pos)) (ParseResult params (add1 pos))
      (let* ([t (check_type_of_param (list-ref Tokens pos) pos)]
            [i (check_name_of_param (list-ref Tokens (ParseResult-nextpos t)) (ParseResult-nextpos t))])
        (collect_params (ParseResult-nextpos i)(append params (list (list (list t i))))))))

(define (check_type_of_param tok pos)
  (if (type_Token? tok)
      (ParseResult (type_Token-value tok) (add1 pos))
      (error "invalid type given, expected a type int or string but read: " (list-ref Tokens pos))))

(define (check_name_of_param tok pos)
  (if (identifier_Token? tok)
      (ParseResult (Variable_Expression (identifier_Token-value tok)) (add1 pos))
      (error "expected a variable, read: " (list-ref Tokens pos))))

(define (parse_function_body_decleration pos)
  (if (leftcurly_Token? (list-ref Tokens pos))
      (collect_body (add1 pos) (list))
      (error "invalid syntax, expected: } but read " (list-ref Tokens pos))))

(define (collect_body pos body)
  (if (rightcurly_Token? (list-ref Tokens pos))
      (ParseResult body (add1 pos))
      (let ([exp_or_stmt (Parse_function pos)])
        (collect_body (ParseResult-nextpos exp_or_stmt) (append body (list exp_or_stmt))))))

(Parse_function 0)






      
  