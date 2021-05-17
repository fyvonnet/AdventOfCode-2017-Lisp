(defpackage :day25
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :fset :empty-set :less :contains? :size :with)
  (:export main))

(in-package :day25)

(defvar *input* NIL)

(defun apply-regex (regex)
  (multiple-value-bind (_ matches) (scan-to-strings regex (pop *input*))
    (aref matches 0)))

(defun state-value (state-str)
  (- (char-code (char state-str 0)) (char-code #\A)))

(defun run-machine (rules state cursor ones-locations steps)
  (if (zerop steps)
    (size ones-locations)
    (destructuring-bind (value-to-write direction new-state)
      (aref rules state (if (contains? ones-locations cursor) 1 0))
      (run-machine
        rules
        new-state
        (+ cursor direction)
        (if (= 1 value-to-write)
          (with ones-locations cursor)
          (less ones-locations cursor))
        (1- steps)))))

(defun main ()
  (read-input-as-list 25 (lambda (l) (unless (zerop (length l)) (push l *input*))))
  (setf *input* (reverse *input*))
  (let*
    ((start-state (state-value (apply-regex "Begin in state \(.\)")))
     (nsteps (parse-integer (apply-regex "after \(.+\) steps")))
     (nstates (/ (length *input*) 9))
     (rules (make-array (list nstates 2))))

    (dotimes (_ nstates)
      (let ((state (state-value (apply-regex "state \(.\)"))))
        (dotimes (_ 2)
          (setf
            (aref rules state (parse-integer (apply-regex "value is \(.\)" )))
            (list
              (parse-integer (apply-regex "value \(.\)" ))
              (let ((direction (apply-regex "the \(.+\)\." )))
                (if (string= direction "left") -1 1))
              (state-value (apply-regex "state \(.\)" )))))))

    (print (run-machine rules start-state 0 (empty-set) nsteps))))

