(defpackage :lemshot/main
  (:nicknames :lemshot)
  (:use :cl :lem :alexandria :lemshot/sprite :lemshot/operation :lemshot/expression))
(in-package :lemshot/main)

(defvar *player*)

(defparameter *player-text* (string-trim '(#\newline #\space) "
+-----
|#####\\
|#####/
+-----
"))

(defparameter *type-a-text* (string-trim '(#\newline #\space) "
+--+
|##|
+--+
"))

(defun compute-size-with-ascii-art (text)
  (flet ((compute-width ()
           (+ 2 (reduce #'max
                        (uiop:split-string text :separator '(#\newline))
                        :start 0
                        :key #'length)))
         (compute-height ()
           (+ 1 (count #\newline text))))
    (values (compute-width) (compute-height))))

(define-attribute player-attribute
  (t :foreground "green" :bold-p t))

(define-attribute shot-attribute
  (t :foreground "red" :bold-p t))

(define-attribute type-a-attribute
  (t :foreground "yellow" :bold-p t))

(defun make-timer-finalizer ()
  (lambda () (stop-timer *running-timer*)))

(defun create-updator (sprite &optional finalizer)
  (lambda ()
    (cond ((alive-sprite-p sprite)
           (update sprite))
          (t
           (funcall finalizer)))))

(defun gameover ()
  (message "GEME OVER"))

;;; player
(defclass player (sprite) ())

(defmethod draw ((player player) point)
  (insert-string point *player-text* :attribute 'player-attribute))

(defmethod update ((player player))
  (dolist (enemy (get-sprites 'enemy))
    (when (collide-p player enemy)
      (delete-sprite player)
      (gameover))))

(defun create-player ()
  (when (and (boundp '*player*)
             (alive-sprite-p *player*))
    (editor-error "alreay exist player"))
  (let ((x (floor (display-width) 2))
        (y (floor (display-height) 2)))
    (multiple-value-bind (w h) (compute-size-with-ascii-art *player-text*)
      (let ((player (create-sprite 'player :x x :y y :width w :height h)))
        (rename-sprite player "player-sprite")
        (setf *player* player)
        player))))

;;; shot
(defclass shot (sprite) ())

(defmethod draw ((shot shot) point)
  (insert-string point "__" :attribute 'shot-attribute))

(defmethod update ((shot shot))
  (shift-sprite shot 1 0)
  (when (<= (display-width) (sprite-x shot))
    (delete-sprite shot))
  (dolist (enemy (get-sprites 'enemy))
    (when (collide-p enemy shot)
      (delete-sprite enemy)
      (delete-sprite shot))))

(defun create-shot-sprite (x y)
  (let ((shot (create-sprite 'shot :x x :y y :width 3 :height 1)))
    (start-timer 5 t (create-updator shot (make-timer-finalizer)))
    shot))

;;; enemy
(defstruct rule initial-x initial-y actions)

(defun get-rule (enemy-name)
  (get enemy-name 'action-rule))

(defun collect-plist-values (plist key)
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

(defmacro def-rule (enemy-name &body spec)
  `(progn
     (setf (get ',enemy-name 'action-rule)
           (make-rule :initial-x ',(getf spec :initial-x)
                      :initial-y ',(getf spec :initial-y)
                      :actions ',(collect-plist-values spec :action)))))

(defgeneric compute-enemy-size (enemy-name))

(defclass enemy (sprite)
  ((operation-queue
    :accessor enemy-operation-queue
    :initarg :operation-queue)
   (current-operation
    :accessor enemy-current-operation
    :initform nil)))

(defun create-enemy (name &optional (action-index 0))
  (multiple-value-bind (width height) (compute-enemy-size name)
    (let* ((rule (get-rule name))
           (enemy (create-sprite name
                                 :x (compute-expression (rule-initial-x rule))
                                 :y (compute-expression (rule-initial-y rule))
                                 :width width
                                 :height height)))
      (setf (enemy-operation-queue enemy)
            (constructor-rule (elt (rule-actions rule) action-index)))
      (next-operation enemy)
      enemy)))

(defun next-operation (enemy)
  (when-let ((operation (pop (enemy-operation-queue enemy))))
    (setf (enemy-current-operation enemy) operation)
    (lemshot/operation::run-operation operation)
    (start-timer (lemshot/operation::every-ms operation)
                 t
                 (create-updator enemy (make-timer-finalizer)))))

(defgeneric execute-operation (operation enemy)
  (:method-combination progn))

(defmethod execute-operation progn ((operation lemshot/operation::<move>) enemy)
  (shift-sprite enemy
                (lemshot/operation::move-dx operation)
                (lemshot/operation::move-dy operation))
  (unless (plusp (decf (lemshot/operation::move-distance operation)))
    (lemshot/operation::finish-operation operation)))

(defmethod update ((enemy enemy))
  (when (minusp (sprite-x enemy))
    (delete-sprite enemy))
  (dolist (shot (get-sprites 'shot))
    (when (collide-p enemy shot)
      (delete-sprite enemy)
      (delete-sprite shot)))
  (let ((operation (enemy-current-operation enemy)))
    (execute-operation operation enemy)
    (when (lemshot/operation::operation-finished-p operation)
      (stop-timer *running-timer*)
      (next-operation enemy))))

;;; typeA
(def-rule type-a
  :initial-x "width"
  :initial-y (/ "height" 4)
  :action ((:left :distance (/ "width" 7) :every 20)
           (:down :distance (/ "height" 5) :every 20)
           (:left :distance "width" :every 20)))

(defclass type-a (enemy)
  ())

(defmethod draw ((type-a type-a) point)
  (insert-string point *type-a-text* :attribute 'type-a-attribute))

(defmethod compute-enemy-size ((name (eql 'type-a)))
  (compute-size-with-ascii-art *type-a-text*))

;;;
(defun player-move-left (player)
  (when (alive-sprite-p player)
    (shift-sprite player -1 0)
    (redraw-display*)))

(defun player-move-right (player)
  (when (alive-sprite-p player)
    (shift-sprite player 1 0)
    (redraw-display*)))

(defun player-move-up (player)
  (when (alive-sprite-p player)
    (shift-sprite player 0 -1)
    (redraw-display*)))

(defun player-move-down (player)
  (when (alive-sprite-p player)
    (shift-sprite player 0 1)
    (redraw-display*)))

(defun player-shot (player)
  (create-shot-sprite (+ (sprite-x player)
                         (sprite-width player))
                      (+ (sprite-y player) 1)))

(defun operate-player (fn)
  (when (alive-sprite-p *player*)
    (funcall fn *player*)
    (update *player*)))

;;;
(define-command lemshot-move-up () ()
  (operate-player 'player-move-up))

(define-command lemshot-move-down () ()
  (operate-player 'player-move-down))

(define-command lemshot-move-left () ()
  (operate-player 'player-move-left))

(define-command lemshot-move-right () ()
  (operate-player 'player-move-right))

(define-command lemshot-shot () ()
  (operate-player 'player-shot))

(define-key *global-keymap* "Left" 'lemshot-move-left)
(define-key *global-keymap* "Right" 'lemshot-move-right)
(define-key *global-keymap* "Up" 'lemshot-move-up)
(define-key *global-keymap* "Down" 'lemshot-move-down)
(define-key *global-keymap* "PageDown" 'lemshot-shot)

(define-command lemshot () ()
  )
