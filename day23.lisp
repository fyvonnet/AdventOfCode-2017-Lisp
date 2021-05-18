(defpackage :day23
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day23)

(defun parse-line (line)
  (destructuring-bind (a . b) (split " " line)
    (cons
      (intern a "KEYWORD")
      (mapcar
        (lambda (x)
          (if (alpha-char-p (char x 0))
            (cons 'reg (- (char-code (char x 0)) (char-code #\a)))
            (cons 'val (parse-integer x))))
        b))))

(defun get-value (x registers)
  (match x
    ((cons 'reg r) (aref registers r))
    ((cons 'val v) v)))

(defun not-prime (x)
  (let ((end (ceiling (sqrt x))))
    (nlet rec ((y 2))
      (unless (> y end)
        (if (zerop (mod x y))
          t
          (rec (1+ y)))))))

(defun main ()
  (let*
    ((program (coerce (read-input-as-list 23 #'parse-line) 'vector))
     (registers (make-array 8 :initial-element 0))
     (len (length program)))
    (nlet rec ((index 0) (mult-count 0))
      (if (or (minusp index) (>= index len))
        (print mult-count)
        (match (aref program index)
          ((list :|set| (cons 'reg r) b)
           (setf (aref registers r) (get-value b registers))
           (rec (1+ index) mult-count))
          ((list :|sub| (cons 'reg r) b)
           (decf (aref registers r) (get-value b registers))
           (rec (1+ index) mult-count))
          ((list :|mul| (cons 'reg r) b)
           (setf (aref registers r) (* (aref registers r) (get-value b registers)))
           (rec (1+ index) (1+ mult-count)))
          ((list :|jnz| a b)
           (rec
             (if (zerop (get-value a registers)) (1+ index) (+ index (get-value b registers)))
             mult-count))))))
  ; using values from my input, not a universal solution:
  (loop for x from 106700 to 123700 by 17
        counting (not-prime x) into cnt
        finally (print cnt)))

