(defpackage :lemshot/utilities
  (:use :cl)
  (:export :trim-whitespaces
           :get-plist-values
           :compute-text-size))
(in-package :lemshot/utilities)

(defun trim-whitespaces (string)
  (string-trim '(#\newline #\space) string))

(defun get-plist-values (plist key)
  (let ((values '()))
    (loop :with value
          :while plist
          :do (setf (values key value plist) (get-properties plist (list key)))
              (cond (key
                     (setf plist (cddr plist))
                     (push value values))
                    (t
                     (return))))
    (nreverse values)))

(defun compute-text-size (text)
  (flet ((compute-width ()
           (+ 2 (reduce #'max
                        (uiop:split-string text :separator '(#\newline))
                        :start 0
                        :key #'length)))
         (compute-height ()
           (+ 1 (count #\newline text))))
    (values (compute-width) (compute-height))))
