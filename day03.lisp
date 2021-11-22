(defpackage :day03
  (:use :cl :aoc-misc aoc-coord)
  (:export main)
  (:import-from :serapeum :nlet))

(in-package :day03)

(defun walk-spiral (n coord dir remsteps &optional (coords nil) (x-min 0) (x-max 0) (y-min 0) (y-max 0) (ndir (floor n 2)))
  (cond
    ((zerop remsteps)
     (list coords x-min x-max y-min y-max))
    ((zerop ndir) (walk-spiral (1+ n) coord (turn 'left dir) remsteps coords x-min x-max y-min y-max))
    (t
      (let ((x (get-x coord)) (y (get-y coord)))
        (walk-spiral
          n
          (next-coord dir coord)
          dir
          (1- remsteps)
          (cons coord coords)
          (if (< x x-min) x x-min)
          (if (> x x-max) x x-max)
          (if (< y y-min) y y-min)
          (if (> y y-max) y y-max)
          (1- ndir))))))

(defun main ()
  (let*
    ((input (parse-integer (first (read-input-as-list 3)))))
    (destructuring-bind (coords x-min x-max y-min y-max) (walk-spiral 2 (make-coord 0 0) 'east input)
      (let*
        ((matrix (make-array (list (- y-max y-min) (- x-max x-min)) :initial-element 0))
         (shift (make-coord (- x-min) (- y-min)))
         (neighbours
           (mapcar
             (lambda (c) (coord+ shift c))
             (list
               '(-1 . -1) '( 0 . -1) '( 1 . -1)
               '(-1 .  0)            '( 1 .  0)
               '(-1 .  1) '( 0 .  1) '( 1 .  1)))))
        (print (manhattan-distance-from-origin (first coords)))
        (setf (aref matrix (get-y shift) (get-x shift)) 1)
        (print
          (nlet rec ((lst (cdr (reverse coords))))
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
                  (rec (cdr lst)))))))))))

