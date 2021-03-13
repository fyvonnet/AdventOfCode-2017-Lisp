
(defpackage :day09
  (:use :cl :aoc-misc)
  (:export main))

(in-package :day09)


(defun read-garbage (lst &optional (count 0))
  (case (car lst)
    (#\! (read-garbage (cddr lst) count))
    (#\> (values (cdr lst) count))
    (otherwise (read-garbage (cdr lst) (1+ count)))))

(defun read-stream (lst &optional (group-score 0) (total-score 0) (garbage-count 0))
  (if (null lst)
    (list total-score garbage-count)
    (case (car lst)
      (#\{ (read-stream (cdr lst) (1+ group-score) total-score garbage-count))
      (#\} (read-stream (cdr lst) (1- group-score) (+ total-score group-score) garbage-count))
      (#\<
       (multiple-value-bind (remaind-lst new-garbage-count) (read-garbage (cdr lst) garbage-count)
         (read-stream remaind-lst group-score total-score new-garbage-count)))
      (otherwise (read-stream (cdr lst) group-score total-score garbage-count)))))

(defun main ()
  (dolist
    (answer (read-stream (coerce (car (read-input-as-list 09)) 'list)))
    (print answer)))

