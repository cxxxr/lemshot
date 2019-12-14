(defpackage :lemshot/operation
  (:use :cl)
  (:import-from :lemshot/expression
                :compute-expression)
  (:export :constructor-rule))
(in-package :lemshot/operation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +operation-states+ '(:suspended :running :dead)))

(defclass <operation> ()
  ((state
    :initform :suspended
    :initarg :state
    :accessor operation-state
    :type #.`(member ,@+operation-states+))))

(defclass <every> ()
  ((ms :initarg :ms :reader every-ms)))

(defclass <move> ()
  ((distance :initarg :distance :accessor move-distance)
   (dx :initarg :dx :reader move-dx)
   (dy :initarg :dy :reader move-dy)))

(defclass <left> (<operation> <every> <move>)
  ()
  (:default-initargs :dx -1 :dy 0))

(defclass <right> (<operation> <every> <move>)
  ()
  (:default-initargs :dx 1 :dy 0))

(defclass <up> (<operation> <every> <move>)
  ()
  (:default-initargs :dx 0 :dy -1))

(defclass <down> (<operation> <every> <move>)
  ()
  (:default-initargs :dx 0 :dy 1))

(defun make-operation (expr)
  (ecase (first expr)
    ((:left :right :up :down)
     (destructuring-bind (&key distance ((:every ms)))
         (rest expr)
       (make-instance (ecase (first expr)
                        (:left '<left>)
                        (:right '<right>)
                        (:up '<up>)
                        (:down '<down>))
                      :distance (compute-expression distance)
                      :ms ms)))))

(defun constructor-rule (rule)
  (mapcar #'make-operation rule))

(defun run-operation (operation)
  (assert (eq (operation-state operation) :suspended))
  (setf (operation-state operation) :running)
  operation)

(defun finish-operation (operation)
  (setf (operation-state operation) :dead)
  operation)

(defun operation-finished-p (operation)
  (eq (operation-state operation) :dead))
