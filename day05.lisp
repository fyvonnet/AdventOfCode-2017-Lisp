(defpackage :day05
  (:use :cl :aoc-misc)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day05)


(defun jump-trampolines (trampolines func)
  (let ((len (length trampolines)))
    (nlet rec ((n 0) (count 0))
      (if (>= n len)
        count
        (let ((v (aref trampolines n)))
          (setf (aref trampolines n) (funcall func v))
          (rec (+ n v) (1+ count)))))))


(defun main ()
  (let
    ((input (read-input-as-list 05 #'parse-integer)))

    (print
      (jump-trampolines
        (coerce input 'vector)
        (lambda (x) (1+ x))))

    (print
      (jump-trampolines
        (coerce input 'vector)
        (lambda (x) (if (>= x 3) (1- x) (1+ x)))))))
