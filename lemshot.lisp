(defpackage :lemshot
  (:use :cl :lem :lemshot.sprite))
(in-package :lemshot)

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

;;; player
(defclass player (sprite) ())

(defmethod draw ((player player) point)
  (insert-string point *player-text* :attribute 'player-attribute))

(defun create-player-sprite ()
  (when (and (boundp '*player*)
             (alive-sprite-p *player*))
    (delete-sprite *player*))
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
    (delete-sprite shot)))

(defun create-shot-sprite (x y)
  (let ((shot (create-sprite 'shot :x x :y y :width 3 :height 1)))
    (start-timer 5 t (create-updator shot (make-timer-finalizer)))
    shot))

;;; enemy
(defclass enemy (sprite) ())

(defmethod update ((enemy enemy))
  (dolist (shot (get-sprites 'shot))
    (when (collide-p enemy shot)
      (delete-sprite enemy)
      (delete-sprite shot))))

;;; typeA
(defclass type-a (enemy) ())

(defmethod draw ((type-a type-a) point)
  (insert-string point *type-a-text* :attribute 'type-a-attribute))

(defun create-type-a-sprite (x y)
  (multiple-value-bind (w h) (compute-size-with-ascii-art *type-a-text*)
    (let ((type-a (create-sprite 'type-a :x x :y y :width w :height h)))
      (start-timer 20 t (create-updator type-a (make-timer-finalizer)))
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

;;;
(define-command lemshot-move-up () ()
  (player-move-up *player*))

(define-command lemshot-move-down () ()
  (player-move-down *player*))

(define-command lemshot-move-left () ()
  (player-move-left *player*))

(define-command lemshot-move-right () ()
  (player-move-right *player*))

(define-command lemshot-shot () ()
  (player-shot *player*))

(define-key *global-keymap* "Left" 'lemshot-move-left)
(define-key *global-keymap* "Right" 'lemshot-move-right)
(define-key *global-keymap* "Up" 'lemshot-move-up)
(define-key *global-keymap* "Down" 'lemshot-move-down)
(define-key *global-keymap* "PageDown" 'lemshot-shot)

(define-command lemshot () ()
  )
