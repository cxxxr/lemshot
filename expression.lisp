(defpackage :lemshot/expression
  (:use :cl)
  (:import-from :lem :display-width :display-height)
  (:export :compute-expression))
(in-package :lemshot/expression)

(defvar *variables* (make-hash-table :test 'equal))

(defmacro register-variable (name form)
  `(setf (gethash ',name *variables*) (lambda () ,form)))

(defun get-variable (name)
  (gethash name *variables*))

(defun expand-expression (expr)
  (cond ((stringp expr)
         (funcall (get-variable expr)))
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
(register-variable "far" most-positive-fixnum)
(register-variable "random-height" (* (random 10) (/ (display-height) 10)))
