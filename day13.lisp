(defpackage :day13
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day13)

(defstruct scanner range (position 1) (moving-forward t))

(defun parse-line (line)
  (multiple-value-bind (_ matches) (scan-to-strings "^(\\d+): (\\d+)$" line)
    (cons 
      (parse-integer (aref matches 0))
      (parse-integer (aref matches 1)))))

(defun create-layers (scanners)
  (let*
    ((nlayers (1+ (apply #'max (mapcar #'car scanners))))
     (layers (make-array (list nlayers) :initial-element nil)))
    (nlet rec ((scans scanners))
      (if (null scans)
        layers
        (destructuring-bind (layer . range) (car scans)
          (setf (aref layers layer) (make-scanner :range range))
          (rec (cdr scans)))))))

(defun update-scanners (layers)
  (loop
    for l across layers
    do (when l
         (if (scanner-moving-forward l)
           (if (= (scanner-range l) (scanner-position l))
             (progn
               (decf (scanner-position l))
               (setf (scanner-moving-forward l) nil))
             (incf (scanner-position l)))
           (if (= 1 (scanner-position l))
             (progn
               (incf (scanner-position l))
               (setf (scanner-moving-forward l) t))
             (decf (scanner-position l)))))))

(defun cross-firewall (layers)
  (let ((len (length layers)))
    (nlet rec ((layer-num 0) (score 0))
      (if (= layer-num len)
        score
        (let*
          ((layer (aref layers layer-num))
           (new-score
             (if (or (null layer) (> (scanner-position layer) 1))
               score
               (+ score (* layer-num (scanner-range layer))))))
          (update-scanners layers)
          (rec (1+ layer-num) new-score))))))

(defun main ()
  (print (cross-firewall (create-layers (read-input-as-list 13 #'parse-line)))))
