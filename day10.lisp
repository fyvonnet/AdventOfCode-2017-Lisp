(defpackage :day10
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day10)


(defun create-vector (len)
  (let 
    ((vec (make-array (list len))))
    (nlet rec ((i 0))
      (if (= i len)
        vec
        (progn
          (setf (aref vec i) i)
          (rec (1+ i)))))))

(defun partial-reverse (vec current-position length)
  (nlet rec ((a current-position) (b (1- (+ current-position length))))
    (when (< a b)
      (let*
        ((a-mod (mod a (length vec)))
         (b-mod (mod b (length vec)))
         (tmp (aref vec a-mod)))
        (setf (aref vec a-mod) (aref vec b-mod))
        (setf (aref vec b-mod) tmp)
        (rec (1+ a) (1- b))))))

(defun hash-round (lengths vec &optional (current-position 0) (skip-size 0))
  (when lengths
    (partial-reverse vec current-position (car lengths))
    (hash-round
      (cdr lengths)
      vec
      (mod (+ current-position (car lengths) skip-size) (length vec))
      (1+ skip-size))))

(defun main ()
  (let
    ((input (mapcar #'parse-integer (split "," (car (read-input-as-list 10)))))
     (vec (create-vector 256)))
    (hash-round input vec)
    (print (* (aref vec 0) (aref vec 1)))))

