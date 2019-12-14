(defpackage :lemshot/expression
  (:use :cl)
  (:import-from :lem :display-width :display-height)
  (:import-from :trivia)
  (:export :compute-expression))
(in-package :lemshot/expression)

(defvar *variables* (make-hash-table :test 'equal))

(defun register-variable (name value)
  (setf (gethash name *variables*) value))

(defun get-variable (name)
  (gethash name *variables*))

(defun expand-expression (expr)
  (cond ((stringp expr)
         (get-variable expr))
        ((atom expr)
         expr)
        (t
         (cons (expand-expression (car expr))
               (expand-expression (cdr expr))))))

(defun compute-expression (expr)
  (let ((result (eval (expand-expression expr))))
    (check-type result number)
    (values (round result))))

(register-variable "width" (display-width))
(register-variable "height" (display-height))
