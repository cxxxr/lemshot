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

(define-attribute player-attribute
  (t :foreground "green" :bold-p t))

(define-attribute shot-attribute
  (t :foreground "red" :bold-p t))

(defclass player (sprite)
  ())

(defclass shot (sprite)
  ())

(defmethod draw ((player player) point)
  (insert-string point *player-text* :attribute 'player-attribute))

(defmethod draw ((shot shot) point)
  (insert-string point "__" :attribute 'shot-attribute))

(defun make-player-sprite ()
  (flet ((compute-width ()
           (+ 2 (reduce #'max (uiop:split-string *player-text* :separator '(#\newline)) :start 0 :key #'length)))
         (compute-height ()
           (+ 1 (count #\newline *player-text*))))
    (when (and (boundp '*player*)
               (alive-sprite-p *player*))
      (delete-sprite *player*))
    (let ((x (floor (display-width) 2))
          (y (floor (display-height) 2))
          (w (compute-width))
          (h (compute-height)))
      (let ((sprite (create-sprite 'player x y w h)))
        (rename-sprite sprite "player-sprite")
        (setf *player* sprite)
        sprite))))

(defun make-shot-updator (shot)
  (lambda ()
    (when (<= (display-width) (sprite-x shot))
      (stop-timer *running-timer*)
      (delete-sprite shot))
    (shift-sprite shot 1 0)
    (redraw-display*)))

(defun make-shot-sprite (x y)
  (let ((shot (create-sprite 'shot x y 3 1)))
    (start-timer 5 t (make-shot-updator shot))))

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
  (make-shot-sprite (+ (sprite-x player)
                       (sprite-width player))
                    (+ (sprite-y player) 1)))

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
