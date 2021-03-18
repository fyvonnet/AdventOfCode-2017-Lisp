(defpackage :day15
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:export main))

(in-package :day15)

(defmacro str-cons (a b) (list 'cons a (list 'lambda '() b)))
(defmacro str-car  (a)   (list 'car a))
(defmacro str-cdr  (a)   (list 'funcall (list 'cdr a)))

(defun parse-line (line)
  (multiple-value-bind (_ matches) (scan-to-strings "^Generator . starts with (\\d+)$" line)
    (parse-integer (aref matches 0))))

(defun stream-generator (val factor)
  (let ((next-value (rem (* val factor) 2147483647)))
    (str-cons next-value (stream-generator next-value factor))))

(defun multiple-stream-generator (stream m)
  (let ((value (str-car stream)))
    (if (zerop (rem value m))
      (str-cons value (multiple-stream-generator (str-cdr stream) m))
      (multiple-stream-generator (str-cdr stream) m))))

(defun count-equals (streams n &optional (count 0))
  (if (zerop n)
    count
    (count-equals
      (mapcar (lambda (str) (str-cdr str)) streams)
      (1- n)
      (if (apply #'= (mapcar (lambda (str) (logand #xFFFF (str-car str))) streams))
        (1+ count)
        count))))

(defun main ()
  (let*
    ((streams
       (mapcar
         #'stream-generator
         (read-input-as-list 15 #'parse-line)
         '(16807 48271)))
     (multiple-streams (mapcar #'multiple-stream-generator streams '(4 8))))
    (print (count-equals streams          40000000))
    (print (count-equals multiple-streams  5000000))))
