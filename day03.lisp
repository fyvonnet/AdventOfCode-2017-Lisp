(defpackage :day03
  (:use :cl :aoc-misc aoc-coord)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day03)

(defun walk-spiral (remsteps &optional (n 2) (coord '(0 . 0)) (dir 'east) (spiral-coords nil) (ndir (floor n 2)))
  (cond
    ((zerop remsteps) spiral-coords)
    ((zerop ndir) (walk-spiral remsteps (1+ n) coord (turn 'left dir) spiral-coords))
    (t
      (let ((x (get-x coord)) (y (get-y coord)))
        (walk-spiral
          (1- remsteps)
          n
          (next-coord dir coord)
          dir
          (cons coord spiral-coords)
          (1- ndir))))))

(defun main ()
  (let*
    ((input (parse-integer (first (read-input-as-list 3))))
     (spiral-coords (walk-spiral input))
     (half-side (destructuring-bind (x . y) (car spiral-coords) (max (abs x) (abs y))))
     (matrix (make-array (list (* 2 half-side) (* 2 half-side)) :initial-element 0))
     (shift (make-coord half-side half-side))
     (neighbours
       (mapcar
         (lambda (c) (coord+ shift c))
         (list
           '(-1 . -1) '( 0 . -1) '( 1 . -1)
           '(-1 .  0)            '( 1 .  0)
           '(-1 .  1) '( 0 .  1) '( 1 .  1)))))
    (print (manhattan-distance-from-origin (first spiral-coords)))
    (setf (aref matrix half-side half-side) 1)
    (print
      (nlet rec ((lst (cdr (reverse spiral-coords))))
        (let
          ((score
             (reduce
               (lambda (s n)
                 (+ s (destructuring-bind (x . y) (coord+ (car lst) n) (aref matrix y x))))
               neighbours :initial-value 0)))
          (if (> score input)
            score
            (progn
              (destructuring-bind (x . y) (coord+ (car lst) shift) (setf (aref matrix y x) score))
              (rec (cdr lst)))))))))

