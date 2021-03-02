(defpackage :day02
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:export main))

(in-package :day02)


(defun checksum1 (lst)
  (labels
    ((rec (l e)
          (if (null l)
            (- (cdr e) (car e))
            (rec (rest l)
                 (cons
                   (min (car e) (first l))
                   (max (cdr e) (first l)))))))
    (rec (rest lst) (cons (first lst) (first lst)))))

(defun checksum2 (lst)
  (labels
    ((rec (a l)
          (let ((b (first l)))
            (when b
              (multiple-value-bind (q r) (floor (max a b) (min a b))
                (if (zerop r)
                  q
                  (rec a (rest l))))))))
    (match (rec (first lst) (rest lst))
           (nil (checksum2 (rest lst)))
           (x x))))

(defun main ()
  (let
    ((input (read-input-as-list 02 (lambda (l) (mapcar #'parse-integer (split "(\\s)+" l))))))
    (dolist (func (list #'checksum1 #'checksum2))
      (print (reduce #'+ (mapcar func input))))))

