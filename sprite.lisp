(defpackage :lemshot.sprite
  (:use :cl :lem)
  (:export :alive-sprite-p
           :sprite
           :draw
           :create-sprite
           :delete-sprite
           :sprite-buffer
           :rename-sprite
           :sprite-x
           :sprite-y
           :sprite-width
           :sprite-height
           :move-sprite
           :shift-sprite))
(in-package :lemshot.sprite)

(defparameter *sprite-counter* 0)

(defvar *sprites* '())

(defclass sprite ()
  ((window
    :initarg :window
    :reader sprite-window)))

(defgeneric draw (sprite point))
(defmethod draw ((sprite sprite) point))

(defun create-sprite (sprite-class x y w h)
  (let* ((sprite-name (format nil "sprite-~D" (incf *sprite-counter*)))
         (buffer (make-buffer sprite-name :temporary t :enable-undo-p nil))
         (window (make-floating-window buffer x y w h nil))
         (sprite (make-instance sprite-class :window window)))
    (push sprite *sprites*)
    (let ((point (buffer-point buffer)))
      (buffer-start point)
      (draw sprite point)
      (buffer-start point))
    sprite))

(defun delete-sprite (sprite)
  (delete-window (sprite-window sprite))
  (setf *sprites* (delete sprite *sprites*))
  (values))

(defun sprite-buffer (sprite)
  (window-buffer (sprite-window sprite)))

(defun rename-sprite (sprite name)
  (buffer-rename (sprite-buffer sprite) name))

(defun alive-sprite-p (sprite)
  (not (deleted-window-p (sprite-window sprite))))

(defun sprite-x (sprite)
  (window-x (sprite-window sprite)))

(defun sprite-y (sprite)
  (window-y (sprite-window sprite)))

(defun sprite-width (sprite)
  (window-width (sprite-window sprite)))

(defun sprite-height (sprite)
  (window-height (sprite-window sprite)))

(defun move-sprite (sprite x y)
  (let ((window (sprite-window sprite)))
    (lem::window-set-pos window x y)))

(defun shift-sprite (sprite dx dy)
  (move-sprite sprite
               (+ (sprite-x sprite) dx)
               (+ (sprite-y sprite) dy)))
