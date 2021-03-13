(defpackage :day11
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :fset :convert :lookup)
  (:export main))

(in-package :day11)


(defun main ()
  (let
    ((dirmap
       (convert
         'fset:map
         '(("n" 1 0 -1) ("ne" 1 -1 0) ("se" 0 -1 1) ("s" -1 0 1) ("sw" -1 1 0) ("nw" 0 1 -1)))))
    (nlet rec ((directions (split "," (car (read-input-as-list 11)))) (coord '(0 0 0)) (furthest 0))
      (let ((distance (/ (apply #'+ (mapcar #'abs coord)) 2)))
        (if (null directions)
          (format t "~a~%~a~%" distance furthest)
          (rec
            (cdr directions)
            (mapcar #'+ coord (lookup dirmap (car directions)))
            (if (> distance furthest) distance furthest)))))))

