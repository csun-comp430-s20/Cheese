#lang racket
(require "FileReader.rkt")
(require "code_generator.rkt")

(with-output-to-file (string-replace input ".txt" ".pl")
  (lambda () (printf compiled)))