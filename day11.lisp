(defpackage :day11
  (:use :cl :aoc-misc :trivia)
  (:import-from :cl-ppcre :split)
  (:export main))

(in-package :day11)


(defun distance (coord)
  (/ (apply #'+ (mapcar #'abs coord)) 2))

(defun main ()
  (destructuring-bind (furthest . coord)
    (reduce 
      (lambda (data direction)
        (destructuring-bind (furthest . coord) data
          (cons
            (let ((dist (distance coord))) (if (> dist furthest) dist furthest))
            (mapcar
              #'+
              coord
              (cond
                ((string= direction "n" ) '( 1  0 -1))
                ((string= direction "ne") '( 1 -1  0))
                ((string= direction "se") '( 0 -1  1))
                ((string= direction "s" ) '(-1  0  1))
                ((string= direction "sw") '(-1  1  0))
                ((string= direction "nw") '( 0  1 -1)))))))
      (split "," (car (read-input-as-list 11)))
      :initial-value '(0 0 0 0))
    (format t "~a~%~a~%" (distance coord) furthest)))

