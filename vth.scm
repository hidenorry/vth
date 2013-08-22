#!/usr/bin/env gosh
(define-module vth
  (use srfi-1)
  (use srfi-13)
  (export vth))
(select-module vth)

(define (line->xy line)
  ;;(line->xy " 3 4")
  (let ((xydata (map x->number
              (string-split (string-trim-both line) #/\s+/))))
    (if (= (length xydata) 2)
        xydata
        (error "not xy data"))))

(define (around-points lis y)
  ;;(around-points '((0 0) (1 2) (3 4) (4 3) (2 1) (1 0)) 1.5e-14) => (((0 0) (1 2)) ((2 1) (1 0)))
  (letrec ((between (lambda (x a b) (or (< a x b) (> a x b))))
           (y-point (lambda (lis) (cadr lis)))
           (rec (lambda (y pre lis acc)
                  (if (null? lis)
                      (reverse acc)
                      (let ((now (car lis))
                            (next (cdr lis)))
                        (if (between y (y-point pre) (y-point now))
                            (rec y now next (cons (list pre now) acc))
                            (rec y now next acc)))))))
    (rec y (car lis) (cdr lis) '())))

(define (linear-rate a b x)
  ;;(linear-rate 3 4 3.1) => 0.8999999999999999
  (/ (- x b) (- a b)))

(define (linear-xpoint lis y)
  ;;(linear-xpoint '((0 1) (-4 3)) 2.)
  ;;(linear-xpoint '((-0.35 1.754080866e-7) (-0.3 5.754699004e-8)) 1.0e-7)
  (letrec ((y-point (lambda (lis) (cadr lis)))
           (x-point (lambda (lis) (car lis)))
           (rec (lambda (lis y)
                  (let ((rate (linear-rate (y-point (first lis))
                                           (y-point (second lis))
                                           y)))
                    (let ((a (x-point (first lis)))
                          (b (x-point (second lis))))
                      (+ (* rate a) (* (- 1 rate) b)))))))
    (rec lis y)))



(define (vth file point)
  (let ((xydata
         (with-input-from-file file
           (lambda ()
             (port-map
              (lambda (line)
                (line->xy line))
              read-line)))))
    (map (lambda (p) (linear-xpoint p point))
         (around-points xydata point))))
