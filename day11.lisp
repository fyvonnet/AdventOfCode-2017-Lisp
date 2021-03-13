(defpackage :day11
  (:use :cl :aoc-misc :trivia)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day11)

(defun main ()
  (nlet rec ((lst (split "," (car (read-input-as-list 11)))) (x 0) (y 0) (z 0) (furthest 0))
    (let ((dist (/ (+ (abs x) (abs y) (abs z)) 2)))
      (match lst
        (nil (format t "~a~%~a~%" dist furthest))
        ((cons fst rst)
         (let* ((new-furthest (if (> dist furthest) dist furthest)))
           (cond
             ((string= fst "n" ) (rec rst (1+ x)     y  (1- z) new-furthest))
             ((string= fst "ne") (rec rst (1+ x) (1- y)     z  new-furthest))
             ((string= fst "se") (rec rst     x  (1- y) (1+ z) new-furthest))
             ((string= fst "s" ) (rec rst (1- x)     y  (1+ z) new-furthest))
             ((string= fst "sw") (rec rst (1- x) (1+ y)     z  new-furthest))
             ((string= fst "nw") (rec rst     x  (1+ y) (1- z) new-furthest)))))))))

