(defpackage :day03
  (:use :cl :aoc-misc aoc-coord)
  (:export main)
  (:import-from :fset
                :lookup
                :empty-map
                :with))

(in-package :day03)

(defun walk-spiral (n coord dir remsteps &optional (ndir (floor n 2)))
  (cond
    ((= 1 remsteps)
     (print (manhattan-distance-from-origin coord))
     (list coord))
    ((zerop ndir) (walk-spiral (1+ n) coord (turn 'left dir) remsteps))
    (t (cons coord (walk-spiral n (next-coord dir coord) dir (1- remsteps) (1- ndir))))))

(defun compute-values (coords input map)
  (let
    ((score
       (apply
         #'+
         (mapcar
           (lambda (s) (lookup map (coord+ s (car coords))))
           (list
             '(-1 . -1) '( 0 . -1) '( 1 . -1)
             '(-1 .  0)            '( 1 .  0)
             '(-1 .  1) '( 0 .  1) '( 1 .  1))))))
    (if (> score input)
      score
      (compute-values (cdr coords) input (with map (car coords) score)))))

(defun main ()
  (let*
    ((input (parse-integer (first (read-input-as-list 3))))
     (coords (walk-spiral 2 (make-coord 0 0) 'east input)))
    (print 
      (compute-values
        (cdr coords)
        input
        (with (empty-map 0) (make-coord 0 0) 1)))))
