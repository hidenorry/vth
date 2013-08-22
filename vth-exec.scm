#!/usr/bin/env gosh

(push! *load-path* (string-append (sys-getenv "HOME") "/apl/at"))
(add-load-path ".")
(use vth)
(use slib-wrapper)
(use srfi-1)

(define (get-files word)
  (filter
   (string->regexp
    (string-append (x->string word)))
   (filter file-is-regular? (sys-readdir "./"))))

(define (main args)
  (let* ((word (second args))
         (leff (x->number (third args)))
         (point (* (/ 1 leff) 1.0e-7)))
    (for-each
     (lambda (f) (format #t "~40,,,' A => ~{~a~}~%" f (vth f point)))
     (get-files word))))





