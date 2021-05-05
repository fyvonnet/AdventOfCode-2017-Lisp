(defpackage :day24
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :fset :contains? :empty-map :empty-set :lookup :size :with)
  (:export main))

(in-package :day24)

(defun decode-line (line)
  (mapcar 
    #'parse-integer
    (split "/" line)))

(defun sort-pair (p)
  (destructuring-bind (a . b) p
    (if (< a b) (cons a b) (cons b a))))

(defvar *max-strength* 0)
(defvar *max-length* 0)
(defvar *max-strength-longest* 0)

(defun use-next-connectors (connectors-map score used start next-connectors)
  (unless (null next-connectors)
    (check-availability
      connectors-map
      (+ score (car next-connectors))
      (with used (sort-pair (cons start (car next-connectors))))
      (car next-connectors))
    (use-next-connectors connectors-map score used start (cdr next-connectors))))

(defun check-availability (connectors-map &optional (score 0) (used (empty-set)) (start 0))
  (let
    ((next-connectors (remove-if (lambda (c) (contains? used (sort-pair (cons start c)))) (lookup connectors-map start)))
     (len (size used)))
    (if (null next-connectors)
      (progn
        (when (> score *max-strength*) (setf *max-strength* score))
        (when
          (or
            (> len *max-length*)
            (and (eq len *max-length*) (> score *max-strength-longest*)))
          (setf *max-length* len)
          (setf *max-strength-longest* score)))
      (use-next-connectors connectors-map (+ score start) used start next-connectors))))

(defun main ()
  (let*
    ((input (read-input-as-list 24 #'decode-line))
     (connectors-map
       (reduce
         (lambda (m l)
           (reduce
             (lambda (m p)
               (with m (car p) (cons (cadr p) (lookup m (car p)))))
             l :initial-value m))
         (list input (remove-if (lambda (p) (= (first p) (second  p))) (mapcar #'reverse input)))
         :initial-value (empty-map))))
    (check-availability connectors-map)
    (print *max-strength*)
    (print *max-strength-longest*)))
