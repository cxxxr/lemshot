(defpackage :lemshot/sprite
  (:use :cl :lem)
  (:import-from :alexandria
                :remove-from-plist)
  (:export :alive-sprite-p
           :sprite
           :draw
           :update
           :create-sprite
           :delete-sprite
           :sprite-buffer
           :rename-sprite
           :sprite-x
           :sprite-y
           :sprite-width
           :sprite-height
           :move-sprite
           :shift-sprite
           :get-sprites
           :collide-p
           :inside-display-p
           :delete-all-sprites))
(in-package :lemshot/sprite)

(defparameter *sprite-counter* 0)

(defvar *sprites* '())

(defclass sprite ()
  ((window
    :initarg :window
    :reader sprite-window)))

(defgeneric draw (sprite point))
(defmethod draw ((sprite sprite) point))

(defgeneric update (sprite))
(defmethod update ((sprite sprite)))

(defun create-sprite (sprite-class &rest initargs &key x y width height &allow-other-keys)
  (let* ((sprite-name (format nil "sprite-~D" (incf *sprite-counter*)))
         (buffer (make-buffer sprite-name :temporary t :enable-undo-p nil))
         (window (make-floating-window buffer x y width height nil))
         (sprite (apply #'make-instance sprite-class
                        :window window
                        (alexandria:remove-from-plist initargs :x :y :width :height))))
    (push sprite *sprites*)
    (let ((point (buffer-point buffer)))
      (buffer-start point)
      (draw sprite point)
      (buffer-start point))
    sprite))

(defun delete-sprite (sprite)
  (when (alive-sprite-p sprite)
    (delete-window (sprite-window sprite))
    (setf *sprites* (delete sprite *sprites*)))
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

(defun get-sprites (&optional (class-name nil class-name-p))
  (if (not class-name-p)
      *sprites*
      (loop :for sprite :in *sprites*
            :when (typep sprite class-name)
            :collect sprite)))

(defun collide-p (a b)
  (and (<= (sprite-x a) (+ (sprite-x b) (sprite-width b)))
       (<= (sprite-x b) (+ (sprite-x a) (sprite-width a)))
       (<= (sprite-y a) (+ (sprite-y b) (sprite-height b)))
       (<= (sprite-y b) (+ (sprite-y a) (sprite-height a)))))

(defun inside-display-p (sprite)
  (and (<= 0 (sprite-x sprite) (1- (display-width)))
       (<= 0 (sprite-y sprite) (1- (display-height)))))

(defun delete-all-sprites ()
  (length (mapc #'delete-sprite *sprites*)))
