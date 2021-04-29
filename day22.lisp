(defpackage :day22
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :fset :contains? :empty-set :less :with)
  (:export main))

(in-package :day22)


(defun add-row (row r set &optional (c 0))
  (if (null row)
    set
    (add-row (cdr row) r (if (char= #\# (car row)) (with set (make-coord c r)) set) (1+ c))))

(defun create-set (rows &optional (r 0) (set (empty-set)))
  (if (null rows)
    set
    (create-set (cdr rows) (1+ r) (add-row (coerce (car rows) 'list) r set))))

(defun work (coord set &optional (direction 'north) (moves 10000) (count 0))
  (if (zerop moves)
    count
    (destructuring-bind (new-direction new-count new-set)
      (if (contains? set coord)
        (list (turn 'right direction)     count  (less set coord))
        (list (turn 'left  direction) (1+ count) (with set coord)))
      (work (next-coord new-direction coord) new-set new-direction (1- moves) new-count))))

(defun main ()
  (let*
    ;((input (read-input-as-list 22 #'identity "test"))
    ((input (read-input-as-list 22))
     (height (length input))
     (width (length (car input)))
     (infected (create-set input))
     (start-coord (apply #'make-coord (mapcar (lambda (x) (floor (/ x 2))) (list width height))))
     )
    (print (work start-coord infected))
    ))
