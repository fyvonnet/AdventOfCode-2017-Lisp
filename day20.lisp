(defpackage :day20
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day20)


(defun parse-line (line)
  (multiple-value-bind (_ matches)
    (scan-to-strings "p=<(.+),(.+),(.+)>, v=<(.+),(.+),(.+)>, a=<(.+),(.+),(.+)>" line)
    (let ((lst (map 'list #'parse-integer matches)))
      (loop for i below 3 collect (subseq lst (* 3 i) (* 3 (1+ i)))))))

(defun numbering (lst &optional (n 0))
  (when lst
    (cons 
      (cons n (car lst))
      (numbering (cdr lst) (1+ n)))))

(defun magnitude (lst)
  (apply #'+ (mapcar #'abs lst)))

(defun compare (a b)
  (let
    ((acc-mag-a (magnitude (fourth a)))
     (acc-mag-b (magnitude (fourth b))))
    (if (= acc-mag-a acc-mag-b)
      (let
        ((vel-mag-a (magnitude (third a)))
         (vel-mag-b (magnitude (third b))))
        (if (= vel-mag-a vel-mag-b)
          (< (magnitude (second a)) (magnitude (second b)))
          (< vel-mag-a vel-mag-b)))
      (< acc-mag-a acc-mag-b))))

(defun main ()
  (let
    ((input (numbering (read-input-as-list 20 #'parse-line))))
    (print (caar (sort input #'compare)))))

