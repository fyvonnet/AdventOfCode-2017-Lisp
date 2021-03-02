(defpackage :day06
  (:use :cl :aoc-misc :trivia)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :fset :empty-map :with :lookup)
  (:export main))

(in-package :day06)


(defun find-max (lst)
  (nlet rec ((max-values (cons 0 (car lst))) (bank 1) (lst (cdr lst)))
    (if (null lst)
      max-values
      (rec
        (if (> (car lst) (cdr max-values))
          (cons bank (car lst))
          max-values)
        (1+ bank)
        (cdr lst)))))

(defun redistribute (lst)
  (match (find-max lst)
    ((cons bank blocks)
     (let
       ((len (length lst))
        (vec (coerce lst 'vector)))
       (setf (aref vec bank) 0)
       (nlet rec ((i (mod (1+ bank) len)) (blocks blocks))
         (if (zerop blocks)
           (coerce vec 'list)
           (progn
             (incf (aref vec i))
             (rec (mod (1+ i) len) (1- blocks)))))))))

(defun find-repeat (lst)
  (nlet rec ((i 0) (lst lst) (map (empty-map)))
    (match (lookup map lst)
      (nil (rec (1+ i) (redistribute lst) (with map lst i)))
      (s (cons i (- i s))))))

(defun main ()
  (match
    (find-repeat
      (mapcar #'parse-integer (split "(\\s+)" (car (read-input-as-list 06)))))
    ((cons answer-1 answer-2) (format t "~a~%~a~%" answer-1 answer-2))))

