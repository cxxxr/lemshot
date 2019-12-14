(defpackage :lemshot/main
  (:nicknames :lemshot)
  (:use :cl :lem :lemshot/sprite))
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

(defun create-player-sprite ()
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
(defclass enemy (sprite)
  ((action-queue :accessor enemy-action-queue
                 :initarg :action-queue)
   (current-action :accessor enemy-current-action
                   :initform nil)))

(defmethod update ((enemy enemy))
  (when (minusp (sprite-x type-a))
    (delete-sprite type-a))
  (dolist (shot (get-sprites 'shot))
    (when (collide-p enemy shot)
      (delete-sprite enemy)
      (delete-sprite shot))))

;;; typeA
(defparameter *type-a-action-rule*
  '((left 10 :every 20)
    '(down 3 :every 20)
    '(left * :every 20)))

(defclass type-a (enemy)
  ()
  (:default-initargs :action-queue *type-a-action-rule*))

(defmethod draw ((type-a type-a) point)
  (insert-string point *type-a-text* :attribute 'type-a-attribute))

(defmethod update ((type-a type-a))
  (call-next-method))

(defun create-type-a-sprite (x y)
  (multiple-value-bind (w h) (compute-size-with-ascii-art *type-a-text*)
    (let ((type-a (create-sprite 'type-a :x x :y y :width w :height h)))
      (start-timer 10 t (create-updator type-a (make-timer-finalizer)))
      type-a)))

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
  (funcall fn *player*)
  (update *player*))

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
