(defpackage :day21
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :fset :empty-map :lookup :with)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day21)


(defun rotate-pattern (pattern &optional output)
  (if (null (car pattern))
    output
    (rotate-pattern
      (mapcar #'cdr pattern)
      (cons
        (mapcar #'car pattern)
        output))))

(defun all-pattern-rotations (pattern &optional (n 4))
  (unless (zerop n)
    (cons
      pattern
      (all-pattern-rotations (rotate-pattern pattern) (1- n)))))

(defun all-pattern-variants (pattern)
  (apply
    #'append
    (mapcar #'all-pattern-rotations (list pattern (mapcar #'reverse pattern)))))

(defun get-positions (size)
  (let ((div (if (zerop (mod size 2)) 2 3)))
    (nlet rec ((n 0) (pos nil))
      (if (= n size)
        (reverse pos)
        (rec (+ div n) (cons (cons n (+ div n)) pos))))))

(defun string-to-pattern (str)
  (mapcar
    (lambda (s) (coerce s 'list))
    (split "/" str)))

(defun enhance-stripe (positions book)
  (lambda (stripe)
    (mapcar ; squares
      (lambda (p) (lookup book p))
      (mapcar ; positions
        (lambda (p)
          (mapcar ; lines
            (lambda (l) (subseq l (car p) (cdr p)))
            stripe))
        positions))))

(defun squares-to-lines (stripe)
  (unless (null (car stripe))
    (cons 
      (apply #'append (mapcar #'car stripe))
      (squares-to-lines (mapcar #'cdr stripe)))))

(defun enhance-pattern (pattern book)
  (let*
    ((len (length pattern))
     (positions (get-positions len))
     (stripes 
       (mapcar
         (lambda (p) (subseq pattern (car p) (cdr p)))
         positions))
     (converted-stripes
       (mapcar (enhance-stripe positions book) stripes)))
    (apply #'append (mapcar #'squares-to-lines converted-stripes))))

(defun multiple-enhancements (n pattern book)
  (if (zerop n)
    pattern
    (multiple-enhancements (1- n) (enhance-pattern pattern book) book)))

(defun add-line-to-rules (book line)
  (destructuring-bind (input output)
    (mapcar #'string-to-pattern (split " => " line))
    (reduce
      (lambda (b i) (with b i output))
      (all-pattern-variants input)
      :initial-value book)))

(defun print-pattern (pattern)
  (mapcar (lambda (l) (format t "~a~%" (coerce l 'string))) pattern))

(defun count-on-pixels (pattern)
  (reduce
    (lambda (tc l) (reduce (lambda (c p) (if (char= p #\#) (1+ c) c)) l :initial-value tc))
    pattern :initial-value 0))

(defun main ()
  (let*
    ((start-pattern (string-to-pattern ".#./..#/###"))
     (book
       (reduce
         #'add-line-to-rules
         (read-input-as-list 21)
         :initial-value (empty-map)))
     (inter-pattern (multiple-enhancements 5 start-pattern book))
     (final-pattern (multiple-enhancements (- 18 5) inter-pattern book)))

    (print (count-on-pixels inter-pattern))
    (print (count-on-pixels final-pattern))))

