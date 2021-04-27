(defpackage :day21
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings :split :regex-replace-all)
  (:import-from :fset :convert :empty-map :lookup :with)
  (:import-from :trivia :match)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day21)

(defun rotate (pattern &optional lines)
  (if (null (first pattern))
    lines
    (rotate
      (mapcar #'cdr pattern)
      (cons (mapcar #'car pattern) lines))))

(defun all-rotations (rots pattern &optional (n 4))
  (if (zerop n)
    rots
    (all-rotations
      (cons (coerce (apply #'append pattern) 'string)  rots)
      (rotate pattern)
      (1- n))))

(defun all-variants (pattern)
  (reduce
    #'all-rotations
    (list pattern (mapcar #'reverse pattern))
    :initial-value nil))

(defun add-line (map line)
  (multiple-value-bind (_ matches) (scan-to-strings "^(.+) => (.+)$" line)
    (reduce
      (lambda (m k) (with m k (regex-replace-all "/" (aref matches 1) "")))
      (all-variants (mapcar (lambda (x) (coerce x 'list)) (split "/" (aref matches 0))))
      :initial-value map)))

(defun make-coordinates (ncoords side corner factor)
  (nlet rec ((n (1- ncoords)) (coords nil))
    (if (= -1 n)
      coords
      (multiple-value-bind (y x) (floor n side)
        (rec
          (1- n)
          (cons
            (cons
              (* (+ (car corner) x) factor)
              (* (+ (cdr corner) y) factor))
            coords))))))

(defun make-corners-coords (squares-per-side square-size)
  (make-coordinates (* squares-per-side squares-per-side) squares-per-side (cons 0 0) square-size))

(defun get-square (pattern corner square-size)
  (let*
    ((npixels (* square-size square-size))
     (str (make-string npixels)))
    (nlet rec ((coords (make-coordinates npixels square-size corner 1)) (n 0))
      (if (null coords)
        str
        (progn
          (setf (aref str n) (aref pattern (cdar coords) (caar coords)))
          (rec (cdr coords) (1+ n)))))))

(defun set-square (pattern corner square-size str)
  (let*
    ((npixels (* square-size square-size)))
    (nlet rec ((coords (make-coordinates npixels square-size corner 1)) (n 0))
      (when coords
        (progn
          (setf (aref pattern (cdar coords) (caar coords)) (aref str n))
          (rec (cdr coords) (1+ n)))))))

(defun update-pattern (dictionary pattern)
  (let*
    ((size (array-dimension pattern 0))
     (div-by-two (zerop (mod size 2)))
     (square-size (if div-by-two 2 3))
     (new-square-size (if div-by-two 3 4))
     (squares-per-side (/ size square-size))
     (corners (make-corners-coords squares-per-side square-size))
     (squares (mapcar (lambda (c) (get-square pattern c square-size)) corners))
     (new-corners (make-corners-coords squares-per-side new-square-size))
     (new-squares (mapcar (lambda (s) (lookup dictionary s)) squares))
     (new-size (* squares-per-side  new-square-size))
     (new-pattern (make-array (list new-size new-size) :element-type 'character)))
    (mapcar (lambda (c s) (set-square new-pattern c new-square-size s)) new-corners new-squares)
    new-pattern))

(defun multiple-updates (dictionary pattern n)
  (if (zerop n)
    pattern
    (multiple-updates dictionary (update-pattern dictionary pattern) (1- n))))

(defun main ()
  (let*
    ((start-pattern
       (make-array '(3 3) :initial-contents '((#\. #\# #\.) (#\. #\. #\#) (#\# #\# #\#))))
     (dictionary 
       (reduce
         #'add-line
         (read-input-as-list 21)
         :initial-value (empty-map)))
    (end-pattern (multiple-updates dictionary start-pattern 18)))
    (loop
      with counter = 0
      for i below (array-dimension end-pattern 0)
      do (loop
           for j below (array-dimension end-pattern 1)
           do (when (char= #\# (aref end-pattern i j)) (incf counter)))
      finally (print counter))))

