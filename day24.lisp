(defpackage :day24
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :fset :contains? :empty-map :empty-set :lookup :size :with)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day24)

(defun decode-line (line)
  (mapcar 
    #'parse-integer
    (split "/" line)))

(defun sort-pair (p)
  (destructuring-bind (a . b) p
    (if (< a b) (cons a b) (cons b a))))

(defun use-next-connectors (connectors-map strength used start next-connectors strength-data)
  (nlet rec ((connectors next-connectors) (strength-data strength-data))
    (if (null connectors)
      strength-data
      (rec
        (cdr connectors)
        (check-availability
          connectors-map
          (+ strength (car connectors))
          (with used (sort-pair (cons start (car connectors))))
          (car connectors)
          strength-data)))))

(defun check-availability (connectors-map &optional (strength 0) (used (empty-set)) (start 0) (strength-data '(0 0 0)))
  (let*
    ((next-connectors (remove-if (lambda (c) (contains? used (sort-pair (cons start c)))) (lookup connectors-map start)))
     (len (size used)))
    (if (null next-connectors)
      (destructuring-bind (max-strength max-length max-strength-longest) strength-data
        (cons
          (if (> strength max-strength) strength max-strength)
          (if
            (or
              (> len max-length)
              (and (eq len max-length) (> strength max-strength-longest)))
            (list len strength)
            (list max-length max-strength-longest))))
      (use-next-connectors connectors-map (+ strength start) used start next-connectors strength-data))))

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
    (destructuring-bind (ans1 _ ans2) (check-availability connectors-map)
      (format t "~a~%~a~%" ans1 ans2))))
