#lang racket

(define in (open-input-file "test.txt"))

(define (fill_text file txt)
  (let ([character (read-char file)])
     (if (not (eof-object? character)) (fill_text file (append txt (list character))) txt)))
   
(define (text file) (fill_text file '()))


(define (item txt pos) (char->integer (list-ref txt pos)))
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

(define (a_token t) (not (void? t)))

(define (Integer_Token txt pos val tokens)
  (let ([num (item txt pos)])
    (if (is_int num)
        (Integer_Token txt (+ pos 1) (concat val num) tokens)
        (when (> (string-length val) 0)
          (tokenizer txt pos (length txt) (append tokens (list (an_int val))))))))

(define (String_Token txt pos val tokens)
  (let ([character (item txt pos)])
    (unless (is_quotation character)
        (String_Token txt (+ pos 1) (concat val character) tokens)
        (when (> (string-length val) 0)
          (tokenizer txt pos (length txt) (append tokens (list (a_string val)) (list "Quotation_token")))))))

(define (Boolean_Token txt pos val tokens)
  (let ([t (take-right txt (- (length txt) pos))])
    (cond
      [(list-prefix? (list #\T #\r #\u #\e) t) (tokenizer txt (+ pos 4) (length txt) (append tokens (list (a_bool (take t 4)))))]
      [(list-prefix? (list #\F #\a #\l #\s #\e) t) (tokenizer txt (+ pos 5) (length txt) (append tokens (list (a_bool (take t 5)))))]
      [else (tokenizer txt pos (length txt) tokens)])))

(define (Quotation_Token txt pos val tokens)
  (let ([q (item txt pos)])
    (if (is_quotation q)
        (Quotation_Token txt (+ pos 1) (concat val q) tokens)
        (when (> (string-length val) 0)
          (String_Token txt pos "" (append tokens (list "Quotation_token")))))))

(define (LeftParen_Token txt pos val tokens)
  (let ([p (item txt pos)])
    (if (is_leftparen p)
        (LeftParen_Token txt (+ pos 1) (concat val p) tokens)
        (when (> (string-length val) 0)
          (tokenizer txt pos (length txt) (append tokens (list "LeftParen_token")))))))

(define (RightParen_Token txt pos val tokens)
  (let ([p (item txt pos)])
    (if (is_rightparen p)
        (RightParen_Token txt (+ pos 1) (concat val p) tokens)
        (when (> (string-length val) 0)
          (tokenizer txt pos (length txt) (append tokens (list "RightParen_token")))))))

(define (tokenizer txt pos end tokens)
  (if (< pos end)
      (let ([c (item txt pos)])
        (cond
          [(or (= c 10) (= c 32)) (tokenizer txt (+ pos 1) end tokens)]
          [(is_leftparen c) (LeftParen_Token txt pos "" tokens)]
          [(is_rightparen c) (RightParen_Token txt pos "" tokens)]
          [(is_quotation c) (Quotation_Token txt pos "" tokens)]
          [(is_int c) (Integer_Token txt pos "" tokens)]
          [else (Boolean_Token txt pos "" tokens)]))
      tokens))

(let ([characters (text in)])
  (tokenizer characters 0 (length characters) '()))
