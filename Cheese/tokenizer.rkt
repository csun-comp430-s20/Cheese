#lang racket

(define in (open-input-file "test.txt"))

(define (fill_text file txt)
  (let ([character (read-char file)])
     (if (not (eof-object? character)) (fill_text file (append txt (list character))) txt)))
   
(define (text file) (fill_text file '()))
(define characters (text in))
(define end (length characters))

(define (item pos) (char->integer (list-ref characters pos)))
(define (to_string n) (string (integer->char n)))
(define (concat val x) (string-append val (to_string x)))

(define (is_int n) (and (> n 47) (< n 58)))
(define (is_letter s) (or (and (> s 64) (< s 91)) (and (> s 96) (< s 123))))
(define (is_quotation q) (= q 34))
(define (is_leftparen p) (= p 40))
(define (is_rightparen p) (= p 41))
(define (is_blank b) (or (= b 10) (= b 32)))
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
(define (is_typeString s) (list-prefix? (list #\s #\t #\r #\i #\n #\g #\space) s))
(define (is_enum e) (list-prefix? (list #\e #\n #\u #\m #\space) e))
(define (is_case c) (list-prefix? (list #\c #\a #\s #\e #\space) c))
(define (is_switch s) (list-prefix? (list #\s #\w #\i #\t #\c #\h #\space) s))
(define (is_default d) (list-prefix? (list #\d #\e #\a #\u #\l #\t #\space) d))
(define (is_other o) (cond
                       [(list? (member o (range 10))) #t]
                       [(list? (member o (range 11 32))) #t]
                       [(list? (member o (list 33 35 36 37 38 39 44 46 47 58 59 63 64 124 126 127))) #t]
                       [(list? (member o (range 91 97))) #t]
                       [else #f]))


(struct Token (type value))

(define (an_int digits) (Token "Integer" digits))
(define (a_string s) (Token "String" s))
(define (a_bool b) (Token "Boolean" (list->string b)))
(define (an_ident i) (Token "Identifier" i))
(define (an_operator o) (Token "Operator" o))
(define a_quotation (Token "Quatation" #\"))

(define (add_token t tokens op) (if (> (string-length t) 0) (append tokens (list (op t))) tokens))

(define (chopped pos) (take-right characters (- end pos)))


(define (Integer_Token pos val tokens)
  (if (< pos end)
    (let ([num (item pos)])
      (if (is_int num)
          (Integer_Token (add1 pos) (concat val num) tokens)
          (Quotation_Token pos "" (add_token val tokens an_int))))
    tokens))

(define (Quotation_Token pos val tokens)
  (if (< pos end)
      (if (is_quotation (item pos))
          (String_Token (add1 pos) "" (append tokens (list a_quotation)))
          (Boolean_Token pos tokens))
      tokens))

(define (String_Token pos val tokens)
  (if (< pos end)
    (let ([character (item pos)])
      (if (not (is_quotation character))
        (String_Token (add1 pos) (concat val character) tokens)
        (Boolean_Token (add1 pos) (append tokens (list (a_string val)) (list a_quotation)))))
    tokens))

(define (Boolean_Token pos tokens)
  (if (< pos end)
    (let ([t (chopped pos)])
      (cond
        [(list-prefix? (list #\T #\r #\u #\e #\space) t) (Boolean_Token (+ pos 4) (append tokens (list (a_bool (take t 4)))))]
        [(list-prefix? (list #\F #\a #\l #\s #\e #\space) t) (Boolean_Token (+ pos 5) (append tokens (list (a_bool (take t 5)))))]
        [else (While_Token pos tokens)]))
    tokens))

(define (While_Token pos tokens)
  (if (< pos end)
        (if (is_while (chopped pos))
            (If_Token (+ pos 5) (append tokens (list (Token "While" "while"))))
            (If_Token pos tokens))
        tokens))

(define (If_Token pos tokens)
  (if (< pos end)
      (if (is_if (chopped pos))
          (Else_Token (+ pos 2) (append tokens (list (Token "If" "if"))))
          (Else_Token pos tokens))
      tokens))

(define (Else_Token pos tokens)
  (if (< pos end)
      (if (is_else (chopped pos))
          (Or_Token (+ pos 4) (append tokens (list (Token "Else" "else"))))
          (Or_Token pos tokens))
      tokens))

(define (Or_Token pos tokens)
  (if (< pos end)
      (if (is_or (chopped pos))
          (Fail_Token (+ pos 2) (append tokens (list (Token "Or" "or"))))
          (Fail_Token pos tokens))
      tokens))

(define (Fail_Token pos tokens)
  (if (< pos end)
      (if (is_fail (chopped pos))
          (Print_Token (+ pos 4) (append tokens (list (Token "Fail" "fail"))))
          (Print_Token pos tokens))
      tokens))

(define (Print_Token pos tokens)
  (if (< pos end)
      (if (is_print (chopped pos))
          (Not_Token (+ pos 5) (append tokens (list (Token "Print" "print"))))
          (Not_Token pos tokens))
      tokens))

(define (Not_Token pos tokens)
  (if (< pos end)
      (if (is_not (chopped pos))
          (And_Token (+ pos 3) (append tokens (list (Token "Not" "not"))))
          (And_Token pos tokens))
  tokens))

(define (And_Token pos tokens)
  (if (< pos end)
      (if (is_and (chopped pos))
          (Type_Token (+ pos 3) (append tokens (list (Token "And" "and"))))
          (Type_Token pos tokens))
  tokens))

(define (Type_Token pos tokens)
  (if (< pos end)
      (cond
        [(is_typeInt (chopped pos)) (Function_Token (+ pos 3) "" (append tokens (list (Token "Type" "int"))))]
        [(is_typeString (chopped pos)) (Function_Token (+ pos 6) "" (append tokens (list (Token "Type" "string"))))]
        [else (Function_Token pos tokens)])
      tokens))

(define (Function_Token pos tokens)
  (if (< pos end)
      (if (is_function (chopped pos))
          (Enum_Token (+ pos 3) (append tokens (list (Token "Function" "def"))))
          (Enum_Token pos tokens))
      tokens))

(define (Enum_Token pos tokens)
  (if (< pos end)
      (if (is_enum (chopped pos))
          (Case_Token (+ pos 4) (append tokens (list (Token "Enum" "enum"))))
          (Case_Token pos tokens))
      tokens))

(define (Case_Token pos tokens)
  (if (< pos end)
      (if (is_case (chopped pos))
          (Switch_Token (+ pos 4) (append tokens (list (Token "Case" "case"))))
          (Switch_Token pos tokens))
      tokens))

(define (Switch_Token pos tokens)
  (if (< pos end)
      (if (is_case (chopped pos))
          (Default_Token (+ pos 6) (append tokens (list (Token "Switch" "switch"))))
          (Default_Token pos tokens))
      tokens))

(define (Default_Token pos tokens)
  (if (< pos end)
      (if (is_default (chopped pos))
          (Identifier_Token (+ pos 7) "" (append tokens (list (Token "Default" "default"))))
          (Identifier_Token pos "" tokens))
      tokens))

(define (Identifier_Token pos val tokens)
  (if (< pos end)
    (let ([i (item pos)])
      (if (is_letter i)
          (Identifier_Token (add1 pos) (concat val i) tokens)
          (LeftParen_Token pos (add_token val tokens an_ident))))
    tokens))

(define (LeftParen_Token pos tokens)
  (if (< pos end)
      (if (is_leftparen (item pos))
          (LeftParen_Token (add1 pos) (append tokens (list (Token "LeftParen" #\())))
          (RightParen_Token pos tokens))
    tokens))

(define (RightParen_Token pos tokens)
  (if (< pos end)
      (if (is_rightparen (item pos))
          (RightParen_Token (add1 pos) (append tokens (list (Token "RightParen" #\)))))
          (Operator_Token pos "" tokens))
    tokens))

(define (Operator_Token pos val tokens)
  (if (< pos end)
      (let ([o (item pos)])
        (if (is_operator o)
            (Operator_Token (add1 pos) (concat val o) tokens)
            (LeftCurlyBracket_Token pos (add_token val tokens an_operator))))
      tokens))

(define (LeftCurlyBracket_Token pos tokens)
  (if (< pos end)
      (if (is_leftCurlyBracket (item pos))
          (LeftCurlyBracket_Token (add1 pos) (append tokens (list (Token "LeftCurlyBracket" #\{))))
          (RightCurlyBracket_Token pos tokens))
  tokens))

(define (RightCurlyBracket_Token pos tokens)
  (if (< pos end)
      (if (is_rightCurlyBracket (item pos))
          (RightCurlyBracket_Token (add1 pos) (append tokens (list (Token "RightCurlyBracket" #\}))))
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

(define (tokenizer pos) (Integer_Token pos "" (list)))
(define Tokens (tokenizer 0))
;(for ([i Tokens])
;  (display (Token-type i))
;  (displayln (Token-value i)))
  
(define amount_of_tokens (length Tokens))
(define (chop_tokens pos) (take-right Tokens (- amount_of_tokens pos)))

(struct If_expression (gaurd ifTrue ifFalse))
(struct Additive_expression (operand primary1 primary2))
(struct Multiplicative_expression (operand primary1 primary2))
(struct Boolean_expression (operand primary1 primary2))
(struct Primary_Expression (value))
(struct While_statement (gaurd body))
(struct Enum_statement (identifier cases))
(struct Or_statement (exp1 exp2))
(struct Switch_statement (exp cases default))
(struct Call_statement (identifier expressions))
(struct Assignment_statement (type identifier exp))
(struct Function_decleration (type identifier body))

(struct Parse_Result (result nextpos))

(define (Parse_function pos)
  (if (< pos amount_of_tokens)
     ;check for left paren
     ;check for function token
     ;parse type token
     ;parse identifier token
     ;check for left paren
     ;parse the parameters
     ;check for right paren
     ;parse body
      ))



          





