(defpackage :day12
  (:use :cl :aoc-misc :cl-ppcre :functional-queue)
  (:import-from :fset :arb :contains? :convert :empty? :less :lookup)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day12)


(defun parse-line (line)
  (multiple-value-bind (_ matches) (scan-to-strings "^(\\d+) <-> (.+)$" line)
    (cons
      (parse-integer (aref matches 0))
      (mapcar #'parse-integer (split ", " (aref matches 1))))))

(defun count-programs (graph programs start)
  (nlet rec ((queue (queue-snoc (empty-queue) start)) (programs (less programs start)) (count 0))
    (if (queue-empty-p queue)
      (values count programs)
      (let* ((next-programs (lookup graph (queue-head queue))))
        (rec
          (reduce
            (lambda (q p) (if (contains? programs p) (queue-snoc q p) q))
            next-programs :initial-value (queue-tail queue))
          (reduce #'less next-programs :initial-value programs)
          (1+ count))))))

(defun count-groups (graph programs &optional (count 1))
  (if (empty? programs)
    count
    (multiple-value-bind (_ programs) (count-programs graph programs (arb programs))
      (count-groups graph programs (1+ count)))))

(defun main ()
  (let*
    ((input (read-input-as-list 12 #'parse-line))
     (programs (convert 'fset:set (mapcar #'car input)))
     (graph    (convert 'fset:map input)))
    (multiple-value-bind (count programs) (count-programs graph programs 0)
      (format t "~a~%~a~%" count (count-groups graph programs)))))

