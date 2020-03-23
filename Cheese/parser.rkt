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
(struct Call_statement (identifier arguments))
(struct Assignment_statement (identifier exp))
(struct Integer_Expression (value))
(struct String_Expression (value))
(struct Boolean_Expression (value))
(struct Variable_Expression (value))
(struct Function (type identifier parameters body))

(struct ParseResult (result nextpos))

(define (is_function_expression f)
  (list-prefix (list leftparen_Token function_Token type_Token identifier_Token leftparen_Token . rightparen_Token leftcurly_Token . rightcurly_Token _ rightparen_Token) f))

(define (Parse_function pos)
  (if (< pos amount_of_tokens)
      (let [f (chopped_tokens pos)]
        (if (is_function_expression f)
            (Function (list-ref f (+ pos 2)) (list-ref f (+ pos 3)) (Parse_Expression (+ pos 5)) (Parse_Expression (+ pos (add1 (index-of f leftcurly_Token)))))
            (Parse_Expression pos)))
      pos))

      
  