#lang racket

;parser for primary (variables, integers, and strings) and logic operators (<, >, <=, >=, ==)
;build an ast

; TODO read in right parenthesis and make tests
;used some function calls from tokenizer.rkt

;structures
(struct Boolean_expression (operand primary1 primary2))
(struct Primary_expression (value))


(struct ParseResult (result nextPos))

;definitions
(define amountOfTokens (length Tokens))

;assuming list of tokens called Tokens is being passed 
;primary can be an integer, string, or variable.
;Primary_expression (value)

(define (isInteger pos)
  (integerToken? (list-ref Tokens (+ 1 pos))))

(define (isVariable pos)
  (variableToken? (list-ref Tokens (+ 1 pos))))

(define (isString pos)
  (stringToken? (list-ref Tokens (+ 1 pos))))

;try to parse primary
(define (parsePrimary pos)
  (if (< pos amountOfTokens)
      ;check if its an integer expression
      (if (isInteger pos)
          ;parse integers
          (ParseResult (Primary_expression (list-ref Tokens pos)) (+ 1 pos))
      ;check if its a variable
      (if (isVariable pos)
          ;parse variables
          (ParseResult (Primary_expression (list-ref Tokens pos)) (+ 1 pos))
      ;check if its a string
      (if (isString pos)
          ;parse strings
          (ParseResult (Primary_expression (list-ref Tokens pos)) (+ 1 pos))
      ;not an integer, variable, or string then return tokens
      Tokens)))))

;less than & greater than. (< p1 p2) (> p1 p2)
;Boolean_Expression (operator p1 p2)
(define (isLessThan pos)
  (and (leftParenToken? (list-ref Tokens pos))
       (eq? "<" (list-ref Tokens (+ 1 pos)))))

(define (isGreaterThan pos)
  (and (leftParenToken? (list-ref Tokens pos))
       (eq? ">" (list-ref Tokens (+ 1 pos)))))

(define (parseLessThan pos)
  (if (< pos amountOfTokens)
      ;less than
      (if (isLessThan pos)
          ;parse lessthan
          (ParseResult (Boolean_expression < (list-ref Tokens (+ 2 pos))
                                           (list-ref Tokens (+ 3 pos))) (+ 1 pos))
      ;if greater than
      (if (isGreaterThan pos)
          (ParseResult (Boolean_expression > (list-ref Tokens (+ 2 pos))
                                           (list-ref Tokens (+ 3 pos))) (+ 1 pos))))
      ;check for right parenthesis
      ;(if (rightParenToken? (list-ref Tokens (ParseResult-nextPos ))
      ))

;less than equal and & greater than equal. (<= p1 p2) (>= p1 p2)
(define (isLessThanEq pos)
  (and (leftParenToken? (list-ref Tokens pos))
       (eq? "<=" (list-ref Tokens (+ 1 pos)))))

(define (isGreaterThanEq pos)
  (and (leftParenToken? (list-ref Tokens pos))
       (eq? ">=" (list-ref Tokens (+ 1 pos)))))

(define (parseLessThanEq pos)
  (if (< pos amountOfTokens)
      ;less than equal
      (if (isLessThanEq pos)
          ;parse less than equal
          (ParseResult (Boolean_expression <= (list-ref Tokens (+ 2 pos))
                                           (list-ref Tokens (+ 3 pos))) (+ 1 pos)))
      (if (isGreaterThanEq pos)
          ;parse greater than equal
          (ParseResult (Boolean_expression >= (list-ref Tokens (+ 2 pos))
                                           (list-ref Tokens (+ 3 pos))) (+ 1 pos)))))

;equivalence. (== p1 p2)
(define (isEquivalence pos)
  (and (leftParenToken? (list-ref Tokens pos))
       (eq? "==" (list-ref Tokens (+ 1 pos)))))

(define (parseEquivalence pos)
  (if (< pos amountOfTokens)
      (if (isEquivalence pos)
          (ParseResult (Boolean_expression == (list-ref Tokens (+ 2 pos))
                                           (list-ref Tokens (+ 3 pos))) (+ 1 pos)))))