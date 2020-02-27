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

(define (an_int digits) (string-append "Integer_token(" digits ")"))
(define (a_string s) (string-append "String_token(" s ")"))
(define (a_bool b) (string-append "Boolean_token(" b ")"))

(define (a_token t) (not (void? t)))

(define (Integer_Token txt pos val tokens)
  (let ([num (item txt pos)])
    (if (is_int num)
        (Integer_Token txt (+ pos 1) (concat val num) tokens)
        (when (> (string-length val) 0)
          (tokenizer txt pos (length txt) (append tokens (list (an_int val))))))))

(define (String_Token txt pos val tokens)
  (let ([character (item txt pos)])
    (if (is_letter character)
        (String_Token txt (+ pos 1) (concat val character) tokens)
        (when (> (string-length val) 0)
          (tokenizer txt pos (length txt) (append tokens (list (a_string val))))))))

(define (Boolean_Token txt pos val)
  (let ([str (String_Token txt pos val)])
    (when (not (void? str))
        (let ([bool (substring str 13 (- (string-length str) 1))])
          (if (is_bool bool) (a_bool bool) str)))))

(define (tokenizer txt pos end tokens)
  (if (< pos end)
      (let ([c (item txt pos)])
        (cond
          [(or (= c 10) (= c 32)) (tokenizer txt (+ pos 1) end tokens)]
          [(is_int c) (Integer_Token txt pos "" tokens)]
          [else (String_Token txt pos "" tokens)]))
      tokens))

(let ([characters (text in)])
  (tokenizer characters 0 (length characters) '()))
