#lang racket

(define in (open-input-file "test.txt"))

(define (fill_text file txt)
  (let ([character (read-char file)])
     (if (not (eof-object? character)) (fill_text file (append txt (list character))) txt)))
   
(define (text file x) (fill_text file x))

(define (tokenizer txt x)
  (if (not (empty? txt)) (let ([character (first txt)]


(text in '())



