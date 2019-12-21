(defpackage :lemshot/operation
  (:use :cl)
  (:import-from :lemshot/expression
                :compute-expression)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :closer-mop)
  (:export :<operation>
           :<every>
           :<repeat>
           :repeat-times
           :<move>
           :move-dx
           :move-dy
           :<left>
           :<right>
           :<up>
           :<down>
           :<beem>
           :<loop>
           :loop-body
           :remake-operation
           :make-operation
           :constructor-rule
           :run-operation
           :finish-operation
           :operation-finished-p
           :get-delay-time))
(in-package :lemshot/operation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +operation-states+ '(:suspended :running :dead)))

(defclass <t> () ())

(defclass <operation> (<t>)
  ((state
    :initform :suspended
    :initarg :state
    :accessor operation-state
    :type #.`(member ,@+operation-states+))))

(defclass <every> (<t>)
  ((ms :initarg :ms :reader every-ms)))

(defclass <repeat> (<t>)
  ((times :initarg :times :accessor repeat-times)))

(defclass <move> (<repeat>)
  ((dx :initarg :dx :reader move-dx)
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

(defclass <beem> (<operation> <every> <repeat>) ())

(defclass <loop> (<operation>)
  ((body :initarg :body :reader loop-body)))

(defun remake-operation (operation)
  (labels ((f (operation)
             (typecase operation
               (<t>
                (apply #'make-instance
                       (type-of operation)
                       :state :suspended
                       (loop :for slot :in (c2mop:class-slots (class-of operation))
                             :for name := (c2mop:slot-definition-name slot)
                             :collect (alexandria:make-keyword name)
                             :collect (f (slot-value operation name)))))
               (otherwise
                operation))))
    (f operation)))

(defun make-operation (expr)
  (ecase (first expr)
    ((:left :right :up :down)
     (destructuring-bind (&key (distance 1) ((:every ms)))
         (rest expr)
       (make-instance (ecase (first expr)
                        (:left '<left>)
                        (:right '<right>)
                        (:up '<up>)
                        (:down '<down>))
                      :times (compute-expression distance)
                      :ms ms)))
    ((:beem)
     (destructuring-bind (&key (repeat 0) ((:every ms) 0))
         (rest expr)
       (make-instance '<beem> :times repeat :ms ms)))
    ((:loop)
     (destructuring-bind (&body body)
         (rest expr)
       (make-instance '<loop> :body (constructor-rule body))))))

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

(defgeneric get-delay-time (operation)
  (:method ((operation <every>)) (every-ms operation))
  (:method ((operation t)) 0))
