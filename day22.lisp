(defpackage :day22
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :fset :empty-map :lookup :with)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day22)

(defun add-row (row r map &optional (c 0))
  (if (null row)
    map
    (add-row
      (cdr row)
      r
      (if (char= #\# (car row)) (with map (make-coord c r) 'infected) map)
      (1+ c))))

(defun create-map (rows &optional (r 0) (map (empty-map 'clean)))
  (if (null rows)
    map
    (create-map
      (cdr rows)
      (1+ r)
      (add-row (coerce (car rows) 'list)r map))))

(defun make-work-func (coord nodes-map)
  (lambda (rules moves)
    (nlet rec ((coord coord) (map nodes-map) (m moves) (direction 'north) (count 0))
      (if (zerop m)
        count
        (destructuring-bind (turn-direction count-increment new-state)
          (cdr (assoc (lookup map coord) rules))
          (let ((new-direction (turn turn-direction direction)))
            (rec
              (next-coord new-direction coord)
              (with map coord new-state)
              (1- m)
              new-direction
              (+ count count-increment))))))))

(defun main ()
  (let*
    ((input (read-input-as-list 22))
     (work
       (make-work-func
         (apply
           #'make-coord
           (mapcar
             (lambda (x) (floor (/ (length x) 2)))
             (list input (car input))))
         (create-map input))))
    (dolist 
      (p '((((infected right 0 clean)
             (clean    left  1 infected)) . 10000)
           (((clean    left  0 weakened)
             (weakened front 1 infected)
             (infected right 0 flagged)
             (flagged  back  0 clean)) . 10000000)))
      (format t "~a~%" (funcall work (car p) (cdr p))))))

