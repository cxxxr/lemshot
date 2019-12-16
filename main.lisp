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

(defun make-timer-finalizer ()
  (lambda () (stop-timer *running-timer*)))

(defun create-updator (sprite &optional (finalizer (make-timer-finalizer)))
  (lambda ()
    (cond ((alive-sprite-p sprite)
           (update sprite))
          (t
           (funcall finalizer)))))

(defun timer-error-handler (condition)
  (pop-up-backtrace condition)
  (stop-timer *running-timer*))

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
    (start-timer 300 t (create-updator explosion) 'timer-error-handler)))

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
  (t :foreground "green" :bold-p t))

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
  (t :foreground "red" :bold-p t))

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
    (start-timer 5 t (create-updator shot) 'timer-error-handler)
    shot))

;;; beem
(define-attribute beem-attribute
  (t :bold-p t))

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
      (start-timer 20 t (create-updator beem) 'timer-error-handler)
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
    ;; (format $ "next-operation: ~A~%" operation)
    (setf (enemy-current-operation enemy) operation)
    (lemshot/operation::run-operation operation)
    (start-timer (lemshot/operation::get-delay-time operation)
                 t
                 (create-updator enemy)
                 'timer-error-handler)))

(defgeneric execute-operation (operation enemy)
  (:method-combination progn))

(defmethod execute-operation progn ((operation lemshot/operation::<repeat>) enemy)
  (unless (plusp (decf (lemshot/operation::repeat-times operation)))
    (lemshot/operation::finish-operation operation)))

(defmethod execute-operation progn ((operation lemshot/operation::<move>) enemy)
  (shift-sprite enemy
                (lemshot/operation::move-dx operation)
                (lemshot/operation::move-dy operation)))

(defmethod execute-operation progn ((operation lemshot/operation::<beem>) enemy)
  (create-beem (sprite-x enemy) (sprite-y enemy)))

(defmethod execute-operation progn ((operation lemshot/operation::<loop>) enemy)
  (let ((body (lemshot/operation::loop-body operation)))
    (setf (enemy-operation-queue enemy)
          (nconc (mapcar #'lemshot/operation::remake-operation body)
                 (cons (lemshot/operation::remake-operation operation)
                       (enemy-operation-queue enemy))))
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
    ;; (format $ "update: ~A~%" operation)
    (execute-operation operation enemy)
    (when (lemshot/operation::operation-finished-p operation)
      (stop-timer *running-timer*)
      (next-operation enemy))))

;;; typeA
(defparameter *type-a-text*
  (lines
   "+--+"
   "|##|"
   "+--+"))

(define-attribute type-a-attribute
  (t :foreground "yellow" :bold-p t))

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
  (t :foreground "orange" :bold-p t))

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
  (t :foreground "orange" :bold-p t))

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
  (t :reverse-p t :foreground "white"))

(define-attribute title-space-attribute
  (t :reverse-p t :foreground "red"))

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
    (stop-timer *running-timer*)
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
      (start-timer 15 t (create-updator title) 'timer-error-hanlder)
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
  (t :reverse-p t :foreground "blue" :background "white"))

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
  (create-shot (+ (sprite-x player)
                         (sprite-width player))
                      (+ (sprite-y player) 1)))

(defun operate-player (fn)
  (when (alive-sprite-p *player*)
    (funcall fn *player*)
    (update *player*)))

;;;
(defparameter *mode* nil)

(defvar *event-queue* (lem::make-event-queue))

(define-global-mode lemshot-mode emacs-mode
  (:keymap *lemshot-keymap*))

(define-command lemshot-start () ()
  (delete-all-sprites)
  (create-player)
  (bt:make-thread 'start-scenario :name "lemshot-enemy-creator-thread")
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
  (setq *mode* nil)
  (emacs-mode))

(defun gameover ()
  (start-timer 3000
               nil
               (lambda ()
                 (lemshot-quit))))

(define-key *lemshot-keymap* "Left" 'lemshot-move-left)
(define-key *lemshot-keymap* "Right" 'lemshot-move-right)
(define-key *lemshot-keymap* "Up" 'lemshot-move-up)
(define-key *lemshot-keymap* "Down" 'lemshot-move-down)
(define-key *lemshot-keymap* "Space" 'lemshot-space)
(define-key *lemshot-keymap* "q" 'lemshot-quit)

(defmacro with-scenario (() &body body)
  (with-unique-names (gf g-out-block-name garg gn g-per-second g-repeat-body g-args)
    `(block ,g-out-block-name
       (flet ((,gf (,garg)
                (unless (eq *mode* :main)
                  (return-from ,g-out-block-name))
                (send-event ,garg)))
         (macrolet ((do-repeat (,gn (&key ((:per-second ,g-per-second) 0)) &body ,g-repeat-body)
                        `(loop :repeat ,,gn
                               :do (progn ,@,g-repeat-body)
                                   (sleep ,,g-per-second)))
                    (create (&rest ,g-args)
                      `(,',gf (lambda () (create-enemy ,@,g-args)))))
           ,@body)))))

(defun start-scenario ()
  (lem::dequeue-event nil *event-queue*)
  (with-scenario ()
    ;; type-a case-1
    (do-repeat 5 (:per-second 0.2)
      (create 'type-a :rule-name 'type-a-case-1))
    (sleep 1)
    (do-repeat 5 (:per-second 0.2)
      (create 'type-a :rule-name 'type-a-case-1))
    (sleep 2)
    ;; type-a case-2
    (do-repeat 5 (:per-second 0.2)
      (create 'type-a :rule-name 'type-a-case-2))
    (sleep 1)
    (do-repeat 5 (:per-second 0.2)
      (create 'type-a :rule-name 'type-a-case-2))
    ;; type-b
    (sleep 2)
    (do-repeat 20 (:per-second 0.3)
      (create 'type-b))))

(define-command lemshot () ()
  (lemshot-mode)
  (setq *mode* :title)
  (delete-all-sprites)
  (create-title))
