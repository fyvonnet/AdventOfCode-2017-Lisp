
(defpackage :day08
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:import-from :fset :with :empty-map :lookup :convert)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day08)


(defun decode-line (line)
  (multiple-value-bind (_ matches)
    (scan-to-strings "^(\\w+) (inc|dec) ([-\\d]+) if (\\w+) ([=><!]+) ([-\\d]+)$" line)
    (match matches
      ((vector register incdec changeby-str regcomp comp valcomp-str)
       (list
         register
         (let ((changeby (parse-integer changeby-str)))
           (if (string= incdec "dec") (- changeby) changeby))
         (lambda (hash)
           (funcall
             (cond
               ((string= comp ">" ) #'> )
               ((string= comp "<" ) #'< )
               ((string= comp ">=") #'>=)
               ((string= comp "<=") #'<=)
               ((string= comp "==") #'= )
               ((string= comp "!=") #'/=))
             (lookup hash regcomp)
             (parse-integer valcomp-str))))))))

(defun run-program (lst &optional (hash (empty-map 0)))
  (if (null lst)
    (apply #'max (mapcar #'cdr (convert 'list hash)))
    (run-program
      (cdr lst)
      (if (funcall (third (car lst)) hash)
        (let ((val (lookup hash (first (car lst)))))
          (with hash (first (car lst)) (+ val (second (car lst)))))
        hash))))

(defun main ()
  (let
    ((input (read-input-as-list 08 #'decode-line)))
    (print (run-program input))))

