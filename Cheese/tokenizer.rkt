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
(define (is_bool b) (or (string=? b "True") (string=? b "False")))
(define (is_quotation q) (= q 34))
(define (is_leftparen p) (= p 40))
(define (is_rightparen p) (= p 41))
(define (is_blank b) (or (= b 10) (= b 32)))
(define (is_operator o) (list? (member o (list 42 43 45 47 60 61 62))))

(define (an_int digits) (string-append "Integer_token(" digits ")"))
(define (a_string s) (string-append "String_token(" s ")"))
(define (a_bool b) (string-append "Boolean_token(" (list->string b) ")"))
(define (an_ident i) (string-append "Identifier_token(" i ")"))
(define (an_operator o) (string-append "Operator_token(" o ")"))

(define (add_token t tokens op) (if (> (string-length t) 0) (append tokens (list (op t))) tokens))

(define (Integer_Token pos val tokens)
  (if (< pos end)
    (let ([num (item pos)])
      (if (is_int num)
          (Integer_Token (+ pos 1) (concat val num) tokens)
          (Quotation_Token pos "" (add_token val tokens an_int))))
    tokens))

(define (Quotation_Token pos val tokens)
  (if (< pos end)
    (let ([q (item pos)])
      (if (is_quotation q)
          (String_Token (add1 pos) "" (append tokens (list "Quotation_token")))
          (Boolean_Token pos tokens)))
    tokens))

(define (String_Token pos val tokens)
  (if (< pos end)
    (let ([character (item pos)])
      (if (not (is_quotation character))
        (String_Token (add1 pos) (concat val character) tokens)
        (Boolean_Token (add1 pos) (append tokens (list (a_string val)) (list "Quotation_token")))))
    tokens))

(define (Boolean_Token pos tokens)
  (if (< pos end)
    (let ([t (take-right characters (- end pos))])
      (cond
        [(list-prefix? (list #\T #\r #\u #\e) t) (Identifier_Token (+ pos 4) "" (add_token (take t 4) tokens a_bool))]
        [(list-prefix? (list #\F #\a #\l #\s #\e) t) (Identifier_Token (+ pos 5) "" (add_token (take t 5) tokens a_bool))]
        [else (Identifier_Token pos "" tokens)]))
    tokens))

(define (LeftParen_Token pos tokens)
  (if (< pos end)
    (let ([p (item pos)])
      (if (is_leftparen p)
          (RightParen_Token (add1 pos) (append tokens (list "LeftParen_token")))
          (RightParen_Token pos tokens)))
    tokens))

(define (RightParen_Token pos tokens)
  (if (< pos end)
    (let ([p (item pos)])
      (if (is_rightparen p)
          (Integer_Token (add1 pos) "" (append tokens (list "RightParen_token")))
          (Operator_Token pos "" tokens)))
    tokens))

(define (Identifier_Token pos val tokens)
  (if (< pos end)
    (let ([i (item pos)])
      (if (is_letter i)
          (Identifier_Token (add1 pos) (concat val i) tokens)
          (LeftParen_Token pos (add_token val tokens an_ident))))
    tokens))

(define (Operator_Token pos val tokens)
  (if (< pos end)
      (let ([o (item pos)])
        (if (is_operator o)
            (Operator_Token (add1 pos) (concat val o) tokens)
            (Blank_Space pos (add_token val tokens an_operator))))
      tokens))

(define (Blank_Space pos tokens)
  (if (< pos end)
    (let ([b (item pos)])
      (if (is_blank b)
          (Blank_Space (add1 pos) tokens)
          (Integer_Token pos "" tokens)))
    tokens))


(define (tokenizer pos tokens)
  (Integer_Token pos "" tokens))

(tokenizer 0 '())
