(defpackage :day11
  (:use :cl :aoc-misc :trivia)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day11)


(defun main ()
  (nlet rec ((directions (split "," (car (read-input-as-list 11)))) (coord '(0 0 0)) (furthest 0))
    (let ((distance (/ (apply #'+ (mapcar #'abs coord)) 2)))
      (match directions
        ('() (format t "~a~%~a~%" distance furthest))
        ((cons first-dir rest-dir)
         (rec
           rest-dir
           (mapcar
             #'+
             coord
             (cond
               ((string= first-dir "n" ) '( 1   0  -1))
               ((string= first-dir "ne") '( 1  -1   0))
               ((string= first-dir "se") '( 0  -1   1))
               ((string= first-dir "s" ) '(-1   0   1))
               ((string= first-dir "sw") '(-1   1   0))
               ((string= first-dir "nw") '( 0   1  -1))))
           (if (> distance furthest) distance furthest)))))))

