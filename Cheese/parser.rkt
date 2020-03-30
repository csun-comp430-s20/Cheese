;begin parsing tokens

;amount of tokens to parse
(define amount_of_tokens (length Tokens))

;structs for the expressions and statements in our language
(struct If_Expression (gaurd ifTrue ifFalse))
(struct Additive_Expression (operand primary1 primary2))
(struct Multiplicative_Expression (operand primary1 primary2))
(struct Boolean_Operation_Expression (operand primary1 primary2))
(struct While_Statement (gaurd body))
(struct Enum_Statement (identifier cases))
(struct Or_Statement (exp1 exp2))
(struct Switch_Statement (exp cases default))
(struct Call_Expression (identifier arguments))
(struct Assignment_Statement (type identifier exp))
(struct Integer_Expression (value))
(struct String_Expression (value))
(struct Boolean_Expression (value))
(struct Variable_Expression (value))
(struct Function_Expression (type identifier parameters body returned))

;struct used to encapsulate expressions and statements and point to the next token (how we're building the ast)
(struct ParseResult (result nextpos))

;helper function, checks if the token at position pos and position pos + 1 are tokens that prefix a function decleration
(define (is_function_expression pos) (and (leftparen_Token? (list-ref Tokens pos)) (function_Token? (list-ref Tokens (add1 pos)))))

;attempts to parse a function
(define (Parse_function pos)
  ;check if there are tokens left
  (if (< pos amount_of_tokens)
      ;check for function decleration prefix: (def 
      (if (is_function_expression pos)
          ;t is the type of the function: int, String, boolean
          (let* ([t (ParseResult (list-ref Tokens (+ pos 2)) (+ pos 3))]
                 ;i is the name of the function, an identifier
                 [i (ParseResult (list-ref Tokens (ParseResult-nextpos t)) (add1 (ParseResult-nextpos t)))]
                 ;p are the parameters
                 [p (parse_parameter_decleration (ParseResult-nextpos i))]
                 ;b is the body
                 [b (parse_function_body_decleration (ParseResult-nextpos p))]
                 ;r is the expression being returned
                 [r (Parse_function (ParseResult-nextpos b))])
            ;check to see if we've reached the end of the function, i.e. is the token at position pos a rightparen_Token
            (if (rightparen_Token? (list-ref Tokens (ParseResult-nextpos r)))
                ;if so retrun a ParseResult containing the Function expression and the next position (pos + 1)
                (ParseResult (Function_Expression t i p b r) (add1 (ParseResult-nextpos r)))
                ;else throw an error due to invalid syntax
                (error "invalid syntax, expected: ) but read: " (list-ref Tokens (ParseResult-nextpos r)))))
            ;(Parse_Expression pos)) to be implemented later when Parse_Expression is created
          ;temporary... verifies there are still more tokens to be parsed
          (if (< (add1 pos) amount_of_tokens) (ParseResult null (add1 pos)) (ParseResult null pos)))
      ;temporary... don't know what else to return if we run out of tokens
      pos))

;prepare to parse function parameters
(define (parse_parameter_decleration pos)
  (if (leftparen_Token? (list-ref Tokens pos))
      (collect_params (add1 pos) (list))
      (error "invalid syntax, expected ) but read : " (list-ref Tokens pos))))

;collects the function parameters
(define (collect_params pos params)
  ;are we done parsing parameters? i.e. has a rightpaen_Token been read if so return a ParseResult with a list of parameters and next position
  (if (rightparen_Token? (list-ref Tokens pos)) (ParseResult params (add1 pos))
      ;t is the type of parameter
      (let* ([t (check_type_of_param (list-ref Tokens pos) pos)]
             ;i is the variable name
            [i (check_name (list-ref Tokens (ParseResult-nextpos t)) (ParseResult-nextpos t))])
        ;recusively call until a rightparen_Token has been read
        (collect_params (ParseResult-nextpos i)(append params (list (list t i)))))))

;verifies the token at position pos is a type_Token and returns a ParseResult containing the type and next position else an error is throw
(define (check_type_of_param tok pos)
  (if (type_Token? tok)
      (ParseResult (type_Token-value tok) (add1 pos))
      (error "invalid type given, expected a type int or string or boolean but read: " (list-ref Tokens pos))))

;verifies the token at position pos is an identifier_Token and returns a ParseResult containing a Variable_Expression and the next position
(define (check_name tok pos)
  (if (identifier_Token? tok)
      (ParseResult (Variable_Expression (identifier_Token-value tok)) (add1 pos))
      (error "expected a variable, read: " (list-ref Tokens pos))))

;prepares to parse a function body
(define (parse_function_body_decleration pos)
  (if (leftcurly_Token? (list-ref Tokens pos))
      (collect_body (add1 pos) (list))
      (error "invalid syntax, expected: } but read " (list-ref Tokens pos))))

;collects the function body
(define (collect_body pos body)
  ;are we done parsing the body? i.e. has a rightculry_Token been read?
  (if (rightcurly_Token? (list-ref Tokens pos))
      ;if so return a ParseResult containing the body (a list of expressions or statements) and the next position
      (ParseResult body (add1 pos))
      ;else continue parsing the body by recursively calling collect_body
      (let ([exp_or_stmt (Parse_function pos)])
        (collect_body (ParseResult-nextpos exp_or_stmt) (append body (list exp_or_stmt))))))

;attempt to parse a function call
(define (Parse_function_call_expression pos)
  ;check if there are still tokens left
  (if (< pos amount_of_tokens)
      ;if the token at position pos and the token at pos + 1 are a leftparen_Token and an identifier_Token
      (if (and (leftparen_Token? (list-ref Tokens pos)) (identifier_Token? (list-ref Tokens (add1 pos))))
          ;then collect the name of the function being called and the arguments provided
          ;i is the name of the function being called
          (let* ([i (ParseResult (Variable_Expression (identifier_Token-value (list-ref Tokens (add1 pos)))) (add1 pos))]
                 ;a are the arguments being passed in 
                 [a (collect_arguments (ParseResult-nextpos i))])
            ;and return a ParseResult of the Call_Expression and next position
            (ParseResult (Call_Expression i a) (ParseResult-nextpos a)))
          ;(Parse_statement pos) to be implemented when Parse_Statement is created
          ;temporary... 
          (ParseResult null pos))
      pos))

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
      (if (rightparen_Token? (list-ref Tokens pos))
          ;if so return a ParseResult of a list of arguments and the next position
          (ParseResult args (add1 pos))
          ;else parse the next argument (an expression) and recursively call collect_arguments until a rightparen_Token has been read
          (let* ([exp (Parse_Expression pos)])
            (collect_arguments (ParseResult-nextpos exp) (append args (list (list exp))))))
      (error "was in the middle of parsing but ran out of tokens")))
          
;temporary
(define (Parse_Expression pos) (display pos))


(define ast (Parse_function 0))

(define branch (ParseResult-result ast))
(ParseResult-result (Function_Expression-type branch))
(ParseResult-result (Function_Expression-identifier branch))
(ParseResult-result (Function_Expression-parameters branch))
(ParseResult-result (Function_Expression-body branch))
(ParseResult-result (Function_Expression-returned branch))
