(defpackage :day13
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :trivia :match)
  (:export main))

(in-package :day13)

(defun parse-line (line)
  (multiple-value-bind (_ matches) (scan-to-strings "^(\\d+): (\\d+)$" line)
    (destructuring-bind (layer range) (mapcar (lambda (i) (parse-integer (aref matches i))) '(0 1))
      (list layer range (* 2 (1- range))))))

(defun severity (layer &optional (delay 0))
  (destructuring-bind (layer-num range divisor) layer
    (when (zerop (rem (+ delay layer-num) divisor)) (* layer-num range))))

(defun undetected-p (firewall delay)
  (cond 
    ((null firewall) t)
    ((null (severity (car firewall) delay))
     (undetected-p (cdr firewall) delay))))

(defun find-delay (firewall &optional (delay 1))
  (if (undetected-p firewall delay)
    delay
    (find-delay firewall (1+ delay))))

(defun main ()
  (let ((input (read-input-as-list 13 #'parse-line)))
    (print
      (reduce
        (lambda (sum layer)
          (match (severity layer) (nil sum) (s (+ sum s))))
        input :initial-value 0))
    (print (find-delay input))))
