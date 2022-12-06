;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'mmt)
(require 'pcase)
(require 'seq)

;;; Input

(defun read-puzzle (f test)
  (funcall f (if (file-exists-p (expand-file-name test))
                 (with-temp-buffer
                   (insert-file-contents test)
                   (buffer-string))
               test)))

(defmacro definput (var f test)
  (declare (indent 2))
  `(progn
     (defvar ,var (read-puzzle ,f ,test))
     ,var))

;;; Shorthands

(defalias 'int 'string-to-number)

(defalias 'char 'string-to-char)

;;; Lists

(defun sum (seq)
  (cl-reduce #'+ seq))

(defun top-n (n list)
  (-take n (sort list #'>)))

(defun join (list)
  (apply #'-concat list))

(defun transpose (lists)
  (apply #'cl-mapcar #'list lists))

(defun windows (n seq)
  (cl-loop for i from 0 below (1+ (- (length seq) n))
           collect (seq-subseq seq i (+ i n))))

;;; Comparison

(defun compare-by (pred key)
  (lambda (x y) (funcall pred (funcall key x) (funcall key y))))

(defun lexicographical-compare (pred &rest rest)
  (let ((preds (cons pred rest)))
    (lambda (x y)
      (cl-loop for (pred . rest) = preds then rest
               when (funcall pred x y)
                 return t
               when (or (not rest) (funcall pred y x))
                 return nil))))

;;; Asserts

(defmacro expect (expr outcome)
  (mmt-once-only (expr outcome)
    `(prog1 ,expr
       (and ,outcome (not (equal ,expr ,outcome))
         (error "Fail! Got: %s, Expected: %s" ,expr ,outcome)))))

;;; Destructuring

(defmacro defun/p (name params &rest body)
  (let ((syms (cl-loop repeat (length params) collect (gensym))))
    `(defun ,name ,syms
       (pcase-let (,@(cl-loop for p in params
                              for s in syms
                              collect `(,p ,s)))
         ,@body))))

(defalias 'lambda/p 'pcase-lambda)

(eval-when-compile
  (defun seq-bindings (params)
    (cl-loop for p in params
             for g = (if (not (symbolp p)) (gensym) p)
             collect (or g p) into syms
             when g
               collect `(,p ,g) into seqs
             finally (return (list syms seqs)))))

(defmacro defun/s (name params &rest body)
  (declare (indent 2))
  (cl-destructuring-bind (syms seqs) (seq-bindings params)
    `(defun ,name ,syms
       (seq-let ,(mapcar #'cl-first seqs) (list ,@(mapcar #'cl-second seqs))
         ,@body))))

(defmacro lambda/s (params &rest body)
  (declare (indent 1))
  (cl-destructuring-bind (syms seqs) (seq-bindings params)
    `(lambda ,syms
       (seq-let ,(mapcar #'cl-first seqs) (list ,@(mapcar #'cl-second seqs))
         ,@body))))

(provide 'aoc-util)
