(defpackage :lemshot/operation
  (:use :cl)
  (:import-from :lem :display-width :display-height)
  (:import-from :trivia)
  (:export :constructor-rule))
(in-package :lemshot/operation)

(defparameter +operation-states+ '(:suspended :running :dead))

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

(defun compute-argument (expr)
  (trivia:ematch expr
    ("width" (display-width))
    ("height" (display-height))
    ((list '/ x y)
     (values (round (compute-argument x) (compute-argument y))))
    (x x)))

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
                      :distance (compute-argument distance)
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
