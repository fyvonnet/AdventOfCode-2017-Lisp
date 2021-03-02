(defpackage :day01
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day01)

(defun read-digits (line)
  (mapcar
    (lambda (c) (parse-integer (coerce (list c) 'string)))
    (coerce line 'list)))

(defun compute-captcha (lst lst2)
  (if (null lst)
    0
    (+
      (if (= (first lst) (first lst2)) (first lst) 0)
      (compute-captcha (rest lst) (rest lst2)))))

(defun drop (n lst)
  (if (zerop n) 
    lst
    (drop (1- n) (rest lst))))

(defun main ()
  (let ((input (first (read-input-as-list 1 #'read-digits))))
    (dolist (func (list #'rest (lambda (l) (drop (/ (length l) 2) l))))
      (print (compute-captcha input (append (funcall func input) input))))))
