#lang racket
;File opener
(display "Enter a file to compile: ")
(define input (read-line (current-input-port)))
(provide input)
(define in (open-input-file input))

(define (fill_text file txt)
  (let ([character (read-char file)])
     (if (not (eof-object? character)) (fill_text file (append txt (list character))) txt)))
   
(define (text file) (fill_text file '()))
(define characters (text in))

(provide characters)