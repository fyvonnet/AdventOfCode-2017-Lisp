(defpackage :day10
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :knot-hash :knot-hash :hash-round)
  (:export main))

(in-package :day10)


(defun create-vector (len)
  (let 
    ((vec (make-array (list len))))
    (nlet rec ((i 0))
      (if (= i len)
        vec
        (progn
          (setf (aref vec i) i)
          (rec (1+ i)))))))

(defun main ()
  (let
    ((input (car (read-input-as-list 10)))
     (vec (create-vector 256)))
    (hash-round (mapcar #'parse-integer (split "," input)) vec 0 0)
    (format t "~a~%" (* (aref vec 0) (aref vec 1)))
    (format t "~a~%" (knot-hash input))))

