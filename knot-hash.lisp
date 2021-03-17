(defpackage :knot-hash
  (:use :cl)
  (:import-from :serapeum :nlet)
  (:export knot-hash hash-round))

(in-package :knot-hash)

(defun create-vector (len)
  (let 
    ((vec (make-array (list len))))
    (nlet rec ((i 0))
      (if (= i len)
        vec
        (progn
          (setf (aref vec i) i)
          (rec (1+ i)))))))

(defun partial-reverse (vec current-position length)
  (nlet rec ((a current-position) (b (1- (+ current-position length))))
    (when (< a b)
      (let*
        ((a-mod (mod a (length vec)))
         (b-mod (mod b (length vec)))
         (tmp (aref vec a-mod)))
        (setf (aref vec a-mod) (aref vec b-mod))
        (setf (aref vec b-mod) tmp)
        (rec (1+ a) (1- b))))))

(defun hash-round (lengths vec current-position skip-size)
  (if lengths
    (progn
      (partial-reverse vec current-position (car lengths))
      (hash-round
        (cdr lengths)
        vec
        (mod (+ current-position (car lengths) skip-size) (length vec))
        (1+ skip-size)))
    (values current-position skip-size)))

(defun slice-list (lst len)
  (when lst
    (cons
      (subseq lst 0 len)
      (slice-list (subseq lst len) len))))

(defun sparse-hash (lengths)
  (let ((vec (create-vector 256)))
    (nlet rec ((rounds 64) (current-position 0) (skip-size 0))
      (if (zerop rounds)
        (coerce vec 'list)
        (multiple-value-bind (cp ss) (hash-round lengths vec current-position skip-size)
          (rec (1- rounds) cp ss))))))

(defun knot-hash (str)
  (let ((lengths (concatenate 'list (map 'list #'char-code str) '(17 31 73 47 23))))
    (string-downcase
      (apply
        #'concatenate
        (cons
          'string
          (mapcar
            (lambda (blk) (format nil "~2,'0x" (apply #'logxor blk)))
            (slice-list (sparse-hash lengths) 16)))))))
