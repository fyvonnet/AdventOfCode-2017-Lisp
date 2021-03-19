(defpackage :day17
  (:use :cl :aoc-misc)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day17)

(defun spinlock (nsteps)
  (nlet rec ((lst '(0)) (pos 0) (value 1))
    (let ((ins-pos (1+ (rem (+ pos nsteps) (length lst)))))
      (if (= value 2017)
        (nth ins-pos lst)
        (rec
          (append (subseq lst 0 ins-pos) (list value) (subseq lst ins-pos))
          ins-pos
          (1+ value))))))

(defun spinlock2 (nsteps)
  (nlet rec ((pos 0) (value 1) (answer nil))
    (if (> value 50000000)
      answer
      (let ((ins-pos (1+ (rem (+ pos nsteps) value))))
        (rec ins-pos (1+ value) (if (= 1 ins-pos) value answer))))))

(defun main ()
  (let
    ((input (car (read-input-as-list 17 #'parse-integer))))
    (print (spinlock  input))
    (print (spinlock2 input))))

