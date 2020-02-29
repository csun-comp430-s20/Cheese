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

(define (an_int digits) (string-append "Integer_token(" digits ")"))
(define (a_string s) (string-append "String_token(" s ")"))
(define (a_bool b) (string-append "Boolean_token(" (list->string b) ")"))
(define (an_ident i) (string-append "Identifier_token(" i ")"))

(define (a_token t) (not (void? t)))

(define (Integer_Token pos val tokens)
  (unless (>= pos end)
    (let ([num (item pos)])
      (if (is_int num)
          (Integer_Token (+ pos 1) (concat val num) tokens)
          (when (> (string-length val) 0)
            (tokenizer pos (append tokens (list (an_int val)))))))))

(define (String_Token pos val tokens)
  (unless (>= pos end)
    (let ([character (item pos)])
      (unless (is_quotation character)
        (String_Token (+ pos 1) (concat val character) tokens)
        (when (> (string-length val) 0)
          (tokenizer (+ pos 2) (append tokens (list "Quotation_token") (list (a_string val)) (list "Quotation_token"))))))))

(define (Boolean_Token pos tokens)
  (unless (>= pos end)
    (let ([t (take-right characters (- end pos))])
      (cond
        [(list-prefix? (list #\T #\r #\u #\e) t) (tokenizer (+ pos 4) (append tokens (list (a_bool (take t 4)))))]
        [(list-prefix? (list #\F #\a #\l #\s #\e) t) (tokenizer (+ pos 5) (append tokens (list (a_bool (take t 5)))))]
        [else (tokenizer pos tokens)]))))

(define (LeftParen_Token pos val tokens)
  (unless (>= pos end)
    (let ([p (item pos)])
      (if (is_leftparen p)
          (LeftParen_Token (add1 pos) (concat val p) tokens)
          (when (> (string-length val) 0)
            (tokenizer pos (append tokens (list "LeftParen_token"))))))))

(define (RightParen_Token pos val tokens)
  (unless (>= pos end)
    (let ([p (item pos)])
      (if (is_rightparen p)
          (RightParen_Token (add1 pos) (concat val p) tokens)
          (when (> (string-length val) 0)
            (tokenizer pos (append tokens (list "RightParen_token"))))))))

(define (Identifier_Token pos val tokens)
  (unless (>= pos end)
    (let ([i (item pos)])
      (if (is_letter i)
          (Identifier_Token (add1 pos) (concat val i) tokens)
          (when (> (string-length val) 0)
            (tokenizer pos (append tokens (list (an_ident val)))))))))

(define (tokenizer pos tokens)
    (unless (>= pos end)
      (let ([c (item pos)])
        (cond
          [(or (= c 10) (= c 32)) (tokenizer (add1 pos) tokens)]
          [(is_quotation c) (String_Token (+ pos 1) "" tokens)]
          [(or (= c 84) (= c 74)) (Boolean_Token pos tokens)]
          [(is_leftparen c) (LeftParen_Token pos "" tokens)]
          [(is_rightparen c) (RightParen_Token pos "" tokens)]
          [(is_int c) (Integer_Token pos "" tokens)]
          [else (Identifier_Token pos "" tokens)])))
  (when (>= pos end) (display tokens)))

(tokenizer 0 '())
