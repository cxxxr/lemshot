(defpackage :lemshot/main
  (:nicknames :lemshot)
  (:use :cl
        :lem
        :alexandria
        :lemshot/utilities
        :lemshot/sprite
        :lemshot/operation
        :lemshot/expression))
(in-package :lemshot/main)

(defparameter *player-text* (trim-whitespaces "
+-----
|#####\\
|#####/
+-----
"))

(defparameter *type-a-text* (trim-whitespaces "
+--+
|##|
+--+
"))

(defparameter *type-b-text* (trim-whitespaces "
 /----|
<#====|
 \----|
"))

(defvar *player*)

(define-attribute player-attribute
  (t :foreground "green" :bold-p t))

(define-attribute shot-attribute
  (t :foreground "red" :bold-p t))

(define-attribute type-a-attribute
  (t :foreground "yellow" :bold-p t))

(define-attribute type-b-attribute
  (t :foreground "red" :bold-p t))

(defun make-timer-finalizer ()
  (lambda () (stop-timer *running-timer*)))

(defun create-updator (sprite &optional (finalizer (make-timer-finalizer)))
  (lambda ()
    (cond ((alive-sprite-p sprite)
           (update sprite))
          (t
           (funcall finalizer)))))

(defun gameover ()
  (message "GEME OVER"))

(defclass object (sprite)
  ((text
    :initarg :text
    :reader object-text)
   (attribute
    :initarg :attribute
    :reader object-attribute)))

(defmethod draw ((object object) point)
  (insert-string point (object-text object) :attribute (object-attribute object)))

;;; explosion
(defclass explosion (object)
  ())

(defmethod update ((explosion explosion))
  (delete-sprite explosion))

(defun gen-explosion-text (width height)
  (trim-whitespaces
   (with-output-to-string (out)
     (dotimes (y height)
       (write-line (make-string (1- width) :initial-element #\@) out)))))

(defun create-explosion (x y width height attribute)
  (let ((explosion
          (create-sprite 'explosion
                         :x x
                         :y y
                         :width width
                         :height height
                         :attribute attribute
                         :text (gen-explosion-text width height))))
    (start-timer 300 t (create-updator explosion))))

(defun create-object-explosion (object)
  (create-explosion (sprite-x object)
                    (sprite-y object)
                    (sprite-width object)
                    (sprite-height object)
                    (object-attribute object)))

(defun kill-enemy (&key enemy shot)
  (delete-sprite enemy)
  (delete-sprite shot)
  (create-object-explosion enemy))

;;; player
(defclass player (object) ()
  (:default-initargs
   :text *player-text*
   :attribute 'player-attribute))

(defun kill-player (player)
  (delete-sprite player)
  (create-object-explosion player)
  (gameover))

(defmethod update ((player player))
  (dolist (enemy (get-sprites 'enemy))
    (when (collide-p player enemy)
      (kill-player player))))

(defun create-player ()
  (when (and (boundp '*player*)
             (alive-sprite-p *player*))
    (editor-error "alreay exist player"))
  (let ((x (compute-expression '(/ "width" 5)))
        (y (compute-expression '(/ "height" 2))))
    (multiple-value-bind (w h) (compute-text-size *player-text*)
      (let ((player (create-sprite 'player :x x :y y :width w :height h)))
        (rename-sprite player "player-sprite")
        (setf *player* player)
        player))))

;;; shot
(defclass shot (object) ()
  (:default-initargs
   :text "__"
   :attribute 'shot-attribute))

(defmethod update ((shot shot))
  (shift-sprite shot 1 0)
  (when (<= (display-width) (sprite-x shot))
    (delete-sprite shot))
  (dolist (enemy (get-sprites 'enemy))
    (when (collide-p enemy shot)
      (kill-enemy :enemy enemy :shot shot))))

(defun create-shot-sprite (x y)
  (let ((shot (create-sprite 'shot :x x :y y :width 3 :height 1)))
    (start-timer 5 t (create-updator shot))
    shot))

;;; enemy
(defstruct rule initial-x initial-y actions)

(defun get-rule (enemy-name)
  (get enemy-name 'action-rule))

(defmacro def-rule (enemy-name &body spec)
  `(progn
     (setf (get ',enemy-name 'action-rule)
           (make-rule :initial-x ',(getf spec :initial-x)
                      :initial-y ',(getf spec :initial-y)
                      :actions ',(get-plist-values spec :action)))))

(defgeneric compute-enemy-size (enemy-name))

(defclass enemy (object)
  ((operation-queue
    :accessor enemy-operation-queue
    :initarg :operation-queue)
   (current-operation
    :accessor enemy-current-operation
    :initform nil)))

(defun create-enemy (name &key (rule-name name) (action-index 0))
  (multiple-value-bind (width height) (compute-enemy-size name)
    (let* ((rule (get-rule rule-name))
           (enemy (create-sprite
                   name
                   :x (compute-expression (rule-initial-x rule))
                   :y (compute-expression (rule-initial-y rule))
                   :width width
                   :height height
                   :operation-queue (constructor-rule
                                     (elt (rule-actions rule)
                                          action-index)))))
      (next-operation enemy)
      enemy)))

(defun next-operation (enemy)
  (when-let ((operation (pop (enemy-operation-queue enemy))))
    (setf (enemy-current-operation enemy) operation)
    (lemshot/operation::run-operation operation)
    (start-timer (lemshot/operation::every-ms operation)
                 t
                 (create-updator enemy))))

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
      (kill-enemy :enemy enemy :shot shot)))
  (dolist (player (get-sprites 'player))
    (when (collide-p enemy player)
      (kill-player player)))
  (let ((operation (enemy-current-operation enemy)))
    (execute-operation operation enemy)
    (when (lemshot/operation::operation-finished-p operation)
      (stop-timer *running-timer*)
      (next-operation enemy))))

;;; typeA
(defclass type-a (enemy) ()
  (:default-initargs
   :attribute 'type-a-attribute
   :text *type-a-text*))

(defmethod compute-enemy-size ((name (eql 'type-a)))
  (compute-text-size *type-a-text*))

;;; typeB
(defclass type-b (enemy) ()
  (:default-initargs
   :attribute 'type-b-attribute
   :text *type-b-text*))

(defmethod compute-enemy-size ((name (eql 'type-b)))
  (compute-text-size *type-b-text*))

;;; rules
(def-rule type-a-case-1
  :initial-x "width"
  :initial-y (/ "height" 4)
  :action ((:left :distance (/ "width" 7) :every 20)
           (:down :distance (/ "height" 5) :every 20)
           (:left :distance "far" :every 20)))

(def-rule type-a-case-2
  :initial-x "width"
  :initial-y (* "height" 3/4)
  :action ((:left :distance (/ "width" 7) :every 20)
           (:up :distance (/ "height" 5) :every 20)
           (:left :distance "far" :every 20)))

(def-rule type-b
  :initial-x "width"
  :initial-y "random-height"
  :action ((:left :distance "far" :every 10)))

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
