(defpackage :day07
  (:use :cl :aoc-misc :cl-ppcre :trivia)
  (:import-from :fset :convert :empty-set :lookup :with)
  (:export main))

(in-package :day07)


(defstruct node total-weight local-weight children)

(defun decode-line (line)
  (multiple-value-bind (_ matches)
    (scan-to-strings "^(\\w+) \\((\\d+)\\)( -> (.*))?" line)
    (match matches
      ((vector nodename weight _ children)
       (list nodename (parse-integer weight) (split ", " children))))))

(defun find-root (lst &optional (name-set (empty-set)) (children-set (empty-set)))
  (if (null lst)
    (car (convert 'list (fset:set-difference name-set children-set)))
    (match (car lst)
      ((list name _ children)
       (find-root
         (cdr lst)
         (with name-set name)
         (reduce #'with children :initial-value children-set))))))

(defun create-tree (hash root)
  (match (lookup hash root)
    ((list weight children)
     (let
       ((children-nodes (mapcar (lambda (c) (create-tree hash c)) children)))
       (make-node
         :total-weight (+ weight (apply #'+ (mapcar #'node-total-weight children-nodes)))
         :local-weight weight
         :children children-nodes)))))

(defun last-two (lst)
  (if (= 2 (length lst))
    lst
    (last-two (cdr lst))))

(defun unbalanced-child (node)
  (let
    ((sorted-children (sort (node-children node) #'< :key #'node-total-weight)))
    (match sorted-children
      ((cons a (cons b _))
       (if (/= (node-total-weight a) (node-total-weight b))
         (cons a (- (node-total-weight a) (node-total-weight b)))
         (match (last-two sorted-children)
           ((list y z)
            (when (/= (node-total-weight y) (node-total-weight z))
              (cons z (- (node-total-weight z) (node-total-weight y)))))))))))

(defun follow-unbalance (tree)
  (match (unbalanced-child tree)
    (nil nil)
    ((cons child unbalance)
     (match (follow-unbalance child)
       (nil (- (node-local-weight child) unbalance))
       (correct-weight correct-weight)))))

(defun main ()
  (let*
    ((input (read-input-as-list 7 #'decode-line))
     (root (find-root input))
     (tree (create-tree (convert 'fset:map input) root)))
    (princ root)
    (print (follow-unbalance tree))))

