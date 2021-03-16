(defpackage :day14
  (:use :cl :aoc-misc :aoc-coord :functional-queue)
  (:import-from :serapeum :integer-range :nlet)
  (:import-from :knot-hash :knot-hash)
  (:import-from :fset :arb :contains? :convert :empty? :less :lookup :size)
  (:export main))

(in-package :day14)

(defun binary-convert (n &optional (i 4) result)
  (if (zerop i)
    result
    (multiple-value-bind (q r) (floor n 2)
      (binary-convert q (1- i) (cons (= 1 r) result)))))

(defun create-binmap (&optional (n 0) lst)
  (if (= 16 n)
    (convert 'fset:map lst)
    (create-binmap
      (1+ n)
      (cons (cons (char "0123456789abcdef" n) (binary-convert n)) lst))))

(defun get-occupied-squares (keystring)
  (let ((binmap (create-binmap)))
    (nlet rec ((n 0) (lst nil))
      (if (= 128 n)
        (convert 'fset:set (apply #'append lst))
        (rec
          (1+ n)
          (cons
            (reduce
              (lambda (l p) (if (cdr p) (cons (make-coord (car p) n) l) l))
              (mapcar 
                #'cons
                (coerce (integer-range 0 128 1) 'list)
                (apply
                  #'append
                  (map
                    'list
                    (lambda (c) (lookup binmap c))
                    (knot-hash (format nil "~a-~a" keystring n)))))
              :initial-value nil)
            lst))))))

(defun explore-region (occupied-squares start)
  (nlet rec ((coords (less occupied-squares start)) (queue (queue-snoc (empty-queue) start)))
    (if (queue-empty-p queue)
      coords
      (let
        ((next-coords
           (remove-if-not
             (lambda (c) (contains? coords c))
             (mapcar
               (lambda (d) (next-coord d (queue-head queue)))
               *all-absolute-dirs*))))
        (rec
          (fset:set-difference coords (convert 'fset:set next-coords))
          (reduce #'queue-snoc next-coords :initial-value (queue-tail queue)))))))

(defun count-regions (occupied-squares &optional (count 0))
  (if (empty? occupied-squares)
    count
    (count-regions
      (explore-region occupied-squares (arb occupied-squares))
      (1+ count))))

(defun main ()
  (let*
    ((occupied-squares (get-occupied-squares (car (read-input-as-list 14)))))
    (print (size occupied-squares))
    (print (count-regions occupied-squares))))

