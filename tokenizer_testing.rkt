#lang racket
;Tokenizer test file
(require rackunit)

;Part 1 of creating a test 
;put characters inside this list 
(define characters (list #\1 #\" #\t #\e #\s #\t #\"))
;jump to line 292 for Part 2 of creating a test 



(define end (length characters))

(define (item pos) (char->integer (list-ref characters pos)))
(define (to_string n) (string (integer->char n)))
(define (concat val x) (string-append val (to_string x)))

(define (is_int n) (and (> n 47) (< n 58)))
(define (is_letter s) (or (and (> s 64) (< s 91)) (and (> s 96) (< s 123))))
(define (is_quotation q) (= q 34))
(define (is_leftparen p) (= p 40))
(define (is_rightparen p) (= p 41))
(define (is_blank b) (or (or (= b 10) (= b 13)) (= b 32)))
(define (is_operator o) (list? (member o (list 42 43 45 47 60 61 62))))
(define (is_while w) (list-prefix? (list  #\w #\h #\i #\l #\e #\space) w))
(define (is_if i) (list-prefix? (list #\i #\f #\space) i))
(define (is_else e) (list-prefix? (list #\e #\l #\s #\e #\space) e))
(define (is_or o) (list-prefix? (list #\o #\r #\space) o))
(define (is_fail f) (list-prefix? (list #\f #\a #\i #\l #\space) f))
(define (is_leftCurlyBracket l) (= l 123))
(define (is_rightCurlyBracket r) (= r 125))
(define (is_print p) (list-prefix? (list #\p #\r #\i #\n #\t #\space) p))
(define (is_not n) (list-prefix? (list #\n #\o #\t #\space) n))
(define (is_and a) (list-prefix? (list #\a #\n #\d #\space ) a))
(define (is_function f) (list-prefix? (list #\d #\e #\f #\space) f))
(define (is_typeInt i) (list-prefix? (list #\i #\n #\t #\space) i))
(define (is_typeString s) (list-prefix? (list #\S #\t #\r #\i #\n #\g #\space) s))
(define (is_typeboolean b) (list-prefix? (list #\b #\o #\o #\l #\e #\a #\n #\space) b))
(define (is_enum e) (list-prefix? (list #\e #\n #\u #\m #\space) e))
(define (is_case c) (list-prefix? (list #\c #\a #\s #\e #\space) c))
(define (is_switch s) (list-prefix? (list #\s #\w #\i #\t #\c #\h #\space) s))
(define (is_default d) (list-prefix? (list #\d #\e #\f #\a #\u #\l #\t #\space) d))
(define (is_other o) (cond
                       [(list? (member o (range 10))) #t]
                       [(list? (member o (range 11 32))) #t]
                       [(list? (member o (list 33 35 36 37 38 39 44 46 47 58 59 63 64 124 126 127))) #t]
                       [(list? (member o (range 91 97))) #t]
                       [else #f]))


(struct integer_Token (value))
(provide (struct-out integer_Token))
(struct string_Token (value))
(provide (struct-out string_Token))
(struct boolean_Token (value))
(provide (struct-out boolean_Token))
(struct identifier_Token (value))
(provide (struct-out identifier_Token))
(struct operator_Token (value))
(provide (struct-out operator_Token))
(struct quotation_Token (default))
(provide (struct-out quotation_Token))
(struct rightparen_Token (default))
(provide (struct-out rightparen_Token))
(struct leftcurly_Token (default))
(provide (struct-out leftcurly_Token))
(struct rightcurly_Token (default))
(provide (struct-out rightcurly_Token))
(struct while_Token (default))
(provide (struct-out while_Token))
(struct if_Token (default))
(provide (struct-out if_Token))
(struct else_Token (default))
(provide (struct-out else_Token))
(struct or_Token (default))
(provide (struct-out or_Token))
(struct fail_Token (default))
(provide (struct-out fail_Token))
(struct print_Token (default))
(provide (struct-out print_Token))
(struct not_Token (default))
(provide (struct-out not_Token))
(struct and_Token (default))
(provide (struct-out and_Token))
(struct type_Token (value))
(provide (struct-out type_Token))
(struct function_Token (default))
(provide (struct-out function_Token))
(struct enum_Token (default))
(provide (struct-out enum_Token))
(struct switch_Token (default))
(provide (struct-out switch_Token))
(struct default_Token (default))
(provide (struct-out default_Token))
(struct case_Token (default))
(provide (struct-out case_Token))
(struct leftparen_Token (default))
(provide (struct-out leftparen_Token))

(define (chopped pos) (take-right characters (- end pos)))


(define (Integer_Token pos val tokens)
  (if (< pos end)
    (let ([num (item pos)])
      (if (is_int num)
          (Integer_Token (add1 pos) (concat val num) tokens)
          (Quotation_Token pos "" (if (> (string-length val) 0) (append tokens (list (integer_Token val))) tokens))))
    tokens))

(define (Quotation_Token pos val tokens)
  (if (< pos end)
      (if (is_quotation (item pos))
          (String_Token (add1 pos) "" tokens)
          (Boolean_Token pos tokens))
      tokens))

(define (String_Token pos val tokens)
  (if (< pos end)
    (let ([character (item pos)])
      (if (not (is_quotation character))
        (String_Token (add1 pos) (concat val character) tokens)
        (Boolean_Token (add1 pos) (if (> (string-length val) 0) (append tokens (list (string_Token val))) tokens))))
    tokens))

(define (Boolean_Token pos tokens)
  (if (< pos end)
    (let ([t (chopped pos)])
      (cond
        [(list-prefix? (list #\T #\r #\u #\e #\space) t) (Boolean_Token (+ pos 4) (append tokens (list (boolean_Token (take t 4)))))]
        [(list-prefix? (list #\F #\a #\l #\s #\e #\space) t) (Boolean_Token (+ pos 5) (append tokens (list (boolean_Token (take t 5)))))]
        [else (While_Token pos tokens)]))
    tokens))

(define (While_Token pos tokens)
  (if (< pos end)
        (if (is_while (chopped pos))
            (If_Token (+ pos 5) (append tokens (list (while_Token #t))))
            (If_Token pos tokens))
        tokens))

(define (If_Token pos tokens)
  (if (< pos end)
      (if (is_if (chopped pos))
          (Else_Token (+ pos 2) (append tokens (list (if_Token #t))))
          (Else_Token pos tokens))
      tokens))

(define (Else_Token pos tokens)
  (if (< pos end)
      (if (is_else (chopped pos))
          (Or_Token (+ pos 4) (append tokens (list (else_Token #t))))
          (Or_Token pos tokens))
      tokens))

(define (Or_Token pos tokens)
  (if (< pos end)
      (if (is_or (chopped pos))
          (Fail_Token (+ pos 2) (append tokens (list (or_Token #t))))
          (Fail_Token pos tokens))
      tokens))

(define (Fail_Token pos tokens)
  (if (< pos end)
      (if (is_fail (chopped pos))
          (Print_Token (+ pos 4) (append tokens (list (fail_Token #t))))
          (Print_Token pos tokens))
      tokens))

(define (Print_Token pos tokens)
  (if (< pos end)
      (if (is_print (chopped pos))
          (Not_Token (+ pos 5) (append tokens (list (print_Token #t))))
          (Not_Token pos tokens))
      tokens))

(define (Not_Token pos tokens)
  (if (< pos end)
      (if (is_not (chopped pos))
          (And_Token (+ pos 3) (append tokens (list (not_Token #t))))
          (And_Token pos tokens))
  tokens))

(define (And_Token pos tokens)
  (if (< pos end)
      (if (is_and (chopped pos))
          (Type_Token (+ pos 3) (append tokens (list (and_Token #t))))
          (Type_Token pos tokens))
  tokens))

(define (Type_Token pos tokens)
  (if (< pos end)
      (cond
        [(is_typeInt (chopped pos)) (Function_Token (+ pos 3) (append tokens (list (type_Token "int"))))]
        [(is_typeString (chopped pos)) (Function_Token (+ pos 6) (append tokens (list (type_Token "String"))))]
        [(is_typeboolean (chopped pos)) (Function_Token (+ pos 7) (append tokens (list (type_Token "boolean"))))]
        [else (Function_Token pos tokens)])
      tokens))

(define (Function_Token pos tokens)
  (if (< pos end)
      (if (is_function (chopped pos))
          (Enum_Token (+ pos 3) (append tokens (list (function_Token #t))))
          (Enum_Token pos tokens))
      tokens))

(define (Enum_Token pos tokens)
  (if (< pos end)
      (if (is_enum (chopped pos))
          (Case_Token (+ pos 4) (append tokens (list (enum_Token #t))))
          (Case_Token pos tokens))
      tokens))

(define (Case_Token pos tokens)
  (if (< pos end)
      (if (is_case (chopped pos))
          (Switch_Token (+ pos 4) (append tokens (list (case_Token #t))))
          (Switch_Token pos tokens))
      tokens))

(define (Switch_Token pos tokens)
  (if (< pos end)
      (if (is_switch (chopped pos))
          (Default_Token (+ pos 6) (append tokens (list (switch_Token #t))))
          (Default_Token pos tokens))
      tokens))

(define (Default_Token pos tokens)
  (if (< pos end)
      (if (is_default (chopped pos))
          (Identifier_Token (+ pos 7) "" (append tokens (list (default_Token #t))))
          (Identifier_Token pos "" tokens))
      tokens))

(define (Identifier_Token pos val tokens)
  (if (< pos end)
    (let ([i (item pos)])
      (if (is_letter i)
          (Identifier_Token (add1 pos) (concat val i) tokens)
          (LeftParen_Token pos (if (> (string-length val) 0) (append tokens (list (identifier_Token val))) tokens))))
    tokens))

(define (LeftParen_Token pos tokens)
  (if (< pos end)
      (if (is_leftparen (item pos))
          (LeftParen_Token (add1 pos) (append tokens (list (leftparen_Token #t))))
          (RightParen_Token pos tokens))
    tokens))

(define (RightParen_Token pos tokens)
  (if (< pos end)
      (if (is_rightparen (item pos))
          (RightParen_Token (add1 pos) (append tokens (list (rightparen_Token #t))))
          (Operator_Token pos "" tokens))
    tokens))

(define (Operator_Token pos val tokens)
  (if (< pos end)
      (let ([o (item pos)])
        (if (is_operator o)
            (Operator_Token (add1 pos) (concat val o) tokens)
            (LeftCurlyBracket_Token pos (if (> (string-length val) 0) (append tokens (list (operator_Token val))) tokens))))
      tokens))

(define (LeftCurlyBracket_Token pos tokens)
  (if (< pos end)
      (if (is_leftCurlyBracket (item pos))
          (LeftCurlyBracket_Token (add1 pos) (append tokens (list (leftcurly_Token #t))))
          (RightCurlyBracket_Token pos tokens))
  tokens))

(define (RightCurlyBracket_Token pos tokens)
  (if (< pos end)
      (if (is_rightCurlyBracket (item pos))
          (RightCurlyBracket_Token (add1 pos) (append tokens (list (rightcurly_Token #t))))
          (Blank_Space pos tokens))
      tokens))

(define (Blank_Space pos tokens)
  (if (< pos end)
      (if (is_blank (item pos))
          (Blank_Space (add1 pos) tokens)
          (Other pos tokens))
      tokens))

(define (Other pos tokens)
  (if (< pos end)
      (if (is_other (item pos))
          (error (concat "invalid character: " (item  pos)))
          (Integer_Token pos "" tokens))
      tokens))
;Part 2 of creating a test
;format for creating a check: (check equal? ({struct type expected}-{a property of the struct type} (first ({name of procedure} {index of character where you want to start checking} "" (list)))) {string representation of expected result})

(check equal? (integer_Token-value (first (Integer_Token 0 "" (list)))) "1")

(check equal? (string_Token-value (first (Quotation_Token 1 "" (list)))) "test")

;Part 3
;if nothing happens after running this file it worked


