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

(defvar *player*)

(defun create-timer-updator (sprite)
  (lambda ()
    (cond ((alive-sprite-p sprite)
           (update sprite))
          (t
           (stop-timer (lem/common/timer:running-timer))))))

(defun timer-error-handler (condition)
  (pop-up-backtrace condition)
  (stop-timer (lem/common/timer:running-timer)))

;;; object
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
    (start-timer (make-timer (create-timer-updator explosion)
                             :handle-function 'timer-error-handler)
                 300
                 t)))

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
(defparameter *player-text*
  (lines
   "+-----"
   "|#####\\"
   "|#####/"
   "+-----"))

(define-attribute player-attribute
  (t :foreground "green" :bold t))

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
(define-attribute shot-attribute
  (t :foreground "red" :bold t))

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

(defun create-shot (x y)
  (let ((shot (create-sprite 'shot :x x :y y :width 3 :height 1)))
    (start-timer (make-timer (create-timer-updator shot) :handle-function 'timer-error-handler)
                 5
                 t)
    shot))

;;; beem
(define-attribute beem-attribute
  (t :bold t))

(defclass beem (object)
  ((vx
    :initarg :vx
    :reader beem-vx)
   (vy
    :initarg :vy
    :reader beem-vy)
   (logical-x
    :initarg :logical-x
    :accessor beem-logical-x)
   (logical-y
    :initarg :logical-y
    :accessor beem-logical-y))
  (:default-initargs
   :text "*"
   :attribute 'beem-attribute))

(defmethod update ((beem beem))
  (incf (beem-logical-x beem) (beem-vx beem))
  (incf (beem-logical-y beem) (beem-vy beem))
  (move-sprite beem
               (round (beem-logical-x beem))
               (round (beem-logical-y beem)))
  (unless (inside-display-p beem)
    (delete-sprite beem))
  (dolist (player (get-sprites 'player))
    (when (collide-p beem player)
      (kill-player player))))

(defun create-beem (x y)
  (let* ((target-x (sprite-x *player*))
         (target-y (sprite-y *player*))
         (vx (if (< x target-x) 1 -1))
         (vy (if (zerop (- target-x x))
                 (if (< y target-y) 1 -1)
                 (* (abs (/ (- target-y y)
                            (- target-x x)))
                    (if (< y target-y) 1 -1)))))
    (let ((beem (create-sprite 'beem
                               :x x
                               :y y
                               :width 2
                               :height 1
                               :vx vx
                               :vy vy
                               :logical-x x
                               :logical-y y)))
      (start-timer (make-timer (create-timer-updator beem) :handle-function 'timer-error-handler)
                   20
                   t)
      beem)))

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

(defun detect-action (rule action-index)
  (elt (rule-actions rule)
       (case action-index
         (:random (random (length (rule-actions rule))))
         (otherwise action-index))))

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
                                     (detect-action rule action-index)))))
      (next-operation enemy)
      enemy)))

(defun next-operation (enemy)
  (when-let ((operation (pop (enemy-operation-queue enemy))))
    (setf (enemy-current-operation enemy) operation)
    (run-operation operation)
    (start-timer (make-timer (create-timer-updator enemy)
                             :handle-function 'timer-error-handler)
                 (get-delay-time operation)
                 t)))

(defgeneric execute-operation (operation enemy)
  (:method-combination progn))

(defmethod execute-operation progn ((operation <repeat>) enemy)
  (unless (plusp (decf (repeat-times operation)))
    (finish-operation operation)))

(defmethod execute-operation progn ((operation <move>) enemy)
  (shift-sprite enemy
                (move-dx operation)
                (move-dy operation)))

(defmethod execute-operation progn ((operation <beem>) enemy)
  (create-beem (sprite-x enemy) (sprite-y enemy)))

(defmethod execute-operation progn ((operation <loop>) enemy)
  (let ((body (loop-body operation)))
    (setf (enemy-operation-queue enemy)
          (nconc (mapcar #'remake-operation body)
                 (cons (remake-operation operation)
                       (enemy-operation-queue enemy))))
    (finish-operation operation)))

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
    (when (operation-finished-p operation)
      (stop-timer (lem/common/timer:running-timer))
      (next-operation enemy))))

;;; typeA
(defparameter *type-a-text*
  (lines
   "+--+"
   "|##|"
   "+--+"))

(define-attribute type-a-attribute
  (t :foreground "yellow" :bold t))

(defclass type-a (enemy) ()
  (:default-initargs
   :attribute 'type-a-attribute
   :text *type-a-text*))

(defmethod compute-enemy-size ((name (eql 'type-a)))
  (compute-text-size *type-a-text*))

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

;;; typeB
(defparameter *type-b-text*
  (lines
   " /----|"
   "<#====|"
   " \\----|"))

(define-attribute type-b-attribute
  (t :foreground "orange" :bold t))

(defclass type-b (enemy) ()
  (:default-initargs
   :attribute 'type-b-attribute
   :text *type-b-text*))

(defmethod compute-enemy-size ((name (eql 'type-b)))
  (compute-text-size *type-b-text*))

(def-rule type-b
  :initial-x "width"
  :initial-y "random-height"
  :action ((:left :distance "far" :every 10)))

;;; typeC
(defparameter *type-c-text*
  (lines
   " ___"
   "////\\"
   "\\___/"))

(define-attribute type-c-attribute
  (t :foreground "orange" :bold t))

(defclass type-c (enemy) ()
  (:default-initargs
   :attribute 'type-c-attribute
   :text *type-c-text*))

(defmethod compute-enemy-size ((name (eql 'type-c)))
  (compute-text-size *type-b-text*))

(def-rule type-c
  :initial-x "width"
  :initial-y "random-height"
  :action ((:loop
             (:left :distance (/ "width" 10) :every 30)
             (:beem)))
  :action ((:loop
             (:left :distance (/ "width" 10) :every 30)
             (:beem :repeat 5 :every 100))))

;;; title
(defparameter *title-text-lines*
  (list
   "                                                                "
   "     #       ######  #    #   ####   #    #   ####    #####     "
   "     #       #       ##  ##  #       #    #  #    #     #       "
   "     #       #####   # ## #   ####   ######  #    #     #       "
   "     #       #       #    #       #  #    #  #    #     #       "
   "     #       #       #    #  #    #  #    #  #    #     #       "
   "     ######  ######  #    #   ####   #    #   ####      #       "
   "                                                                "))

(define-attribute title-character-attribute
  (t :reverse t :foreground "white"))

(define-attribute title-space-attribute
  (t :reverse t :foreground "red"))

(defclass title (sprite)
  ((fix-position
    :initarg :fix-position
    :reader title-fix-position)))

(defmethod draw ((title title) point)
  (dolist (string *title-text-lines*)
    (loop :for start := 0 :then (1+ pos)
          :for pos := (position #\# string :start start)
          :while pos
          :do (insert-string point (make-string (- pos start) :initial-element #\space)
                             :attribute 'title-space-attribute)
              (insert-string point " " :attribute 'title-character-attribute)
          :finally (insert-string point (make-string (1+ (- (length string) start))
                                                     :initial-element #\space)
                                  :attribute 'title-space-attribute))
    (insert-character point #\newline)))

(defmethod update ((title title))
  (shift-sprite title 1 0)
  (when (>= (sprite-x title)
            (title-fix-position title))
    (stop-timer (lem/common/timer:running-timer))
    (create-menu)))

(defun create-title ()
  (multiple-value-bind (w h) (compute-text-size (apply #'lines *title-text-lines*))
    (let* ((x 0)
           (y (compute-expression `(/ "height" 4)))
           (title (create-sprite 'title
                                 :x x
                                 :y y
                                 :width w
                                 :height h
                                 :fix-position (compute-expression
                                                `(- (/ "width" 2)
                                                    (/ ,w 2))))))
      (start-timer (make-timer (create-timer-updator title) :handle-function 'timer-error-handler)
                   15
                   t)
      title)))

;;; menu
(defparameter *menu-text*
  (lines
   "                                    "
   "                                    "
   "      Space   shot                  "
   "      Left    Move player left      "
   "      Right   Move player right     "
   "       Up     Move player up        "
   "      Down    Move player down      "
   "                                    "
   "                                    "
   "        Press Space to start        "
   "                                    "
   "                                    "))

(define-attribute menu-attribute
  (t :reverse t :foreground "blue" :background "white"))

(defclass menu (sprite)
  ())

(defmethod draw ((menu menu) point)
  (insert-string point *menu-text* :attribute 'menu-attribute))

(defun create-menu ()
  (multiple-value-bind (w h) (compute-text-size *menu-text*)
    (let ((menu
            (create-sprite 'menu
                           :x (compute-expression `(- (/ "width" 2) (/ ,w 2)))
                           :y (compute-expression `(- (/ "height" 2) (/ ,h 2)))
                           :width w
                           :height h)))
      menu)))

;;;
(defun player-move-left (player)
  (when (alive-sprite-p player)
    (shift-sprite player -1 0)))

(defun player-move-right (player)
  (when (alive-sprite-p player)
    (shift-sprite player 1 0)))

(defun player-move-up (player)
  (when (alive-sprite-p player)
    (shift-sprite player 0 -1)))

(defun player-move-down (player)
  (when (alive-sprite-p player)
    (shift-sprite player 0 1)))

(defun player-shot (player)
  (create-shot (+ (sprite-x player)
                         (sprite-width player))
                      (+ (sprite-y player) 1)))

(defun operate-player (fn)
  (when (alive-sprite-p *player*)
    (funcall fn *player*)
    (update *player*)))

;;;
(defparameter *mode* nil)

(defvar *event-queue* (lem-core::make-event-queue))

(define-global-mode lemshot-mode (lem-core::emacs-mode)
  (:keymap *lemshot-keymap*))

(define-command lemshot-start () ()
  (delete-all-sprites)
  (create-player)
  (start-scenario)
  (setq *mode* :main)
  (send-event t *event-queue*))

(define-command lemshot-move-up () ()
  (operate-player 'player-move-up))

(define-command lemshot-move-down () ()
  (operate-player 'player-move-down))

(define-command lemshot-move-left () ()
  (operate-player 'player-move-left))

(define-command lemshot-move-right () ()
  (operate-player 'player-move-right))

(define-command lemshot-space (arg) ("p")
  (case *mode*
    (:title (lemshot-start))
    (:main (operate-player 'player-shot))
    (otherwise (self-insert arg))))

(define-command lemshot-quit () ()
  (delete-all-sprites)
  (delete-all-scenario-timers)
  (setq *mode* nil)
  (lem-core::emacs-mode))

(defun gameover ()
  (start-timer (make-timer (lambda ()
                             (lemshot-quit)
                             (message "Game Over")))
               3000 nil))

(define-key *lemshot-keymap* "Left" 'lemshot-move-left)
(define-key *lemshot-keymap* "Right" 'lemshot-move-right)
(define-key *lemshot-keymap* "Up" 'lemshot-move-up)
(define-key *lemshot-keymap* "Down" 'lemshot-move-down)
(define-key *lemshot-keymap* "Space" 'lemshot-space)
(define-key *lemshot-keymap* "q" 'lemshot-quit)

(defun run-reservation-timers (reservation-timers)
  (mapc #'funcall (reverse reservation-timers)))

(defvar *scenario-reservation-timers* '())
(defvar *scenario-current-ms* 0)
(defvar *created-timers* '())

(defun delete-all-scenario-timers ()
  (dolist (timer *created-timers*)
    (stop-timer timer))
  (setq *created-timers* '()))

(defun reserve-scenario-timer (fn)
  (push (let ((ms *scenario-current-ms*))
          (lambda ()
            (push (start-timer (make-timer fn) ms)
                  *created-timers*)))
        *scenario-reservation-timers*))

(defmacro with-scenario ((&key loop) &body body)
  (once-only (loop)
    (with-unique-names (g-repeat g-per-second g-repeat-body g-create-args)
      `(let ((*scenario-current-ms* 0)
             (*scenario-reservation-timers* '()))
         (flet ((pause (second)
                  (incf *scenario-current-ms* (* second 1000))))
           (macrolet ((do-repeat (,g-repeat (&key ((:per-second ,g-per-second) 0)) &body ,g-repeat-body)
                        `(loop :repeat ,,g-repeat
                               :do (progn ,@,g-repeat-body)
                                   (pause ,,g-per-second)))
                      (create (&rest ,g-create-args)
                        `(reserve-scenario-timer
                          (lambda ()
                            (if (eq *mode* :main)
                                (create-enemy ,@,g-create-args)
                                (stop-timer (lem/common/timer:running-timer)))))))
             ,@body))
         (run-reservation-timers *scenario-reservation-timers*)
         (when ,loop
           (push (start-timer (make-timer (let ((timers *scenario-reservation-timers*))
                                            (lambda ()
                                              (if (eq *mode* :main)
                                                  (run-reservation-timers timers)
                                                  (stop-timer (lem/common/timer:running-timer))))))
                              *scenario-current-ms*
                              t)
                 *created-timers*))))))

(defun start-scenario ()
  (with-scenario (:loop t)
    (labels ((type-a-case-1 ()
               (pause 2)
               (do-repeat 5 (:per-second 0.2)
                 (create 'type-a :rule-name 'type-a-case-1)))
             (type-a-case-2 ()
               (pause 2)
               (do-repeat 5 (:per-second 0.2)
                 (create 'type-a :rule-name 'type-a-case-2)))
             (type-b ()
               (pause 2)
               (do-repeat 10 (:per-second 0.2)
                 (create 'type-b)))
             (type-c ()
               (pause 2)
               (do-repeat 5 (:per-second 1)
                 (create 'type-c :action-index :random))))
      (type-a-case-1)
      (type-a-case-2)
      (type-b)
      (type-c)
      (random-case
        (type-b)
        (type-c)
        (type-a-case-1)
        (type-a-case-2)))))

(define-command lemshot () ()
  (lemshot-mode)
  (setq *mode* :title)
  (delete-all-sprites)
  (create-title))
