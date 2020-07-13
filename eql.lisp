;;;; Equilibrium/CL
;;;;
;;;; A port of a video game I once wrote in 2010 in Forth, for the SDL
;;;; 1.2 library.  I wonder how easy it will be to port the software
;;;; to the Common Lisp environment?
;;;;
;;;; NOTE NOTE NOTE: You cannot run this software from the SLIME IDE.
;;;; SLIME keeps the SBCL (or whatever) instance running indefinitely;
;;;; SDL2 **expects** the process to die after SDL_Quit() is called,
;;;; because it fails to clean up all the resources that are acquired
;;;; by SDL_Init() and/or other SDL2 functions.  This can be observed
;;;; by noting the first time you run the program, things seems to
;;;; work well; but the second time around, you get SDL_RC_ERROR
;;;; errors when attempting to initialize the library.


(ql:quickload :sdl2)


;;; State of the game's universe.  You have a playfield, a player,
;;; a handful of opponents, and so forth.

(defparameter *playfield* nil)
(defparameter *player* nil)
(defparameter *opponents* nil)
(defparameter *sparks* nil)


;;; Players, opponents, and sparks all have on-screen behavior.
;;; These generic functions specify what these behaviors are.

(defgeneric paint-with-renderer (p r))
(defgeneric move-object (p &key halfstep))
(defgeneric border-collisions (p))
(defgeneric border-collision-consequence (p x y pf))
(defgeneric handle-bumps (p1 p2))
(defgeneric move-to (p x y))
(defgeneric init (p))


(defmethod move-object (p &key halfstep)
  (declare (ignore p halfstep))
  ; unless otherwise specified, objects are immobile.
  (values))

(defmethod move-to (p x y)
  (declare (ignore p x y))
  ; unless otherwise specified, objects are immobile.
  (values))

(defmethod border-collisions (p)
  ; unless otherwise specified, objects have no borders.
  (values))

(defmethod handle-bumps (p1 p2)
  ; unless otherwise specified, it is an error to 'bump'
  ; one object against another unless they're both players
  ; or opponents.
  (error "should never be called."))


;;; The playfield represents the most distantly visible object
;;; on the screen.  It is literally the field on which the
;;; other actors in the game reside upon.

(defclass playfield ()
  ((w :initarg :w :reader w :initform (error ":w required"))
   (h :initarg :h :reader h :initform (error ":h required"))))

(defmethod paint-with-renderer ((p playfield) r)
  (sdl2:set-render-draw-color r 0 0 0 255)
  (sdl2:render-clear r)
  (sdl2:set-render-draw-color r 255 255 255 255)
  (sdl2:render-draw-rect r (sdl2:make-rect 0 0 (w p) (h p)))
  (values))


;;; Player objects represent the human player or one or more
;;; computer-driven opponents.  The human player is always
;;; white; opponents' colors are chosen randomly, such that
;;; they should always be visible and always be distinct from
;;; the human player.

(defun random-color (&key (range 128) (offset 64))
  (let ((r (+ (random range *random-state*) offset))
        (g (+ (random range *random-state*) offset))
        (b (+ (random range *random-state*) offset)))
    (list r g b 255)))

(defun random-xy (dimension length)
  (let* ((xy (random dimension *random-state*))
         (constrained-xy (max 0 (- xy length))))
    constrained-xy))

(defun center-xy (dimension length)
  (/ (- dimension length) 2))

(defun right (p)
  (+ (left p) (w p)))

(defun bottom (p)
  (+ (top p) (h p)))

(defun h-rebound (p)
  (setf (dx p) (- (dx p))))

(defun v-rebound (p)
  (setf (dy p) (- (dy p))))

(defun avg (a b)
  (floor (/ (+ a b) 2)))

(defclass player ()
  ((left :initarg :left :accessor left)
   (top :initarg :top :accessor top)
   (w :initform 16 :reader w)
   (h :initform 16 :reader h)
   (dx :accessor dx :initform 0)
   (dy :accessor dy :initform 0)
   (color :reader color :initform '(255 255 255 255))
   (pf :initarg :on-playfield :initform nil :reader playfield-of)))

(defclass opponent (player)
  ((color :accessor color)))

(defmethod initialize-instance :after ((p player) &key)
  (init p))

(defmethod init ((p player))
  (let ((pf (playfield-of p)))
    (move-to p (center-xy (w pf) (w p))
               (center-xy (h pf) (h p)))))

(defmethod init ((o opponent))
  (setf (color o) (random-color))
  (setf (dx o) (- (random 7 *random-state*) 3))
  (setf (dy o) (- (random 7 *random-state*) 3))
  (move-to o (random-xy (w (playfield-of o)) (w o))
             (random-xy (h (playfield-of o)) (h o))))

(defmethod paint-with-renderer ((p player) r)
  (let ((left (left p))
        (top (top p))
        (right (right p))
        (bottom (bottom p))
        (color (color p)))
    (apply #'sdl2:set-render-draw-color (append (list r) color))
    (sdl2:render-draw-line r left top right top)
    (sdl2:render-draw-line r right top right bottom)
    (sdl2:render-draw-line r right bottom left bottom)
    (sdl2:render-draw-line r left bottom left top)
    (values)))

(defmethod move-to ((p player) x y)
  (let ((x (floor x))
        (y (floor y)))
    (setf (left p) x)
    (setf (top p) y))
  (values))

(defmethod move-object ((p player) &key halfstep)
  (let* ((scale (if halfstep 0.5 1.0))
         (new-x (* scale (+ (left p) (dx p))))
         (new-y (* scale (+ (top p) (dy p)))))
    (move-to p new-x new-y))
  (values))

(defmethod border-collisions ((p player))
  ; let-block originally intended to help in debugging an issue.
  ; However, in retrospect, this is better b/c it's faster to run.
  (let ((player-right (right p))
        (player-left (left p))
        (player-top (top p))
        (player-bottom (bottom p))
        (field-left 0)
        (field-right (w (playfield-of p)))
        (field-top 0)
        (field-bottom (h (playfield-of p)))
        (pf (playfield-of p)))
    (cond ((>= player-right field-right)
           ; We've exceeded the right edge by N pixels.  Moving back
           ; by N pixels puts us right on the edge, which isn't realistic.
           ; Move by another N pixels to properly emulate the reflection
           ; off the wall.
           (decf (left p) (* 2 (- player-right field-right)))
           (border-collision-consequence
	     p player-right (avg player-top player-bottom) pf)
           (h-rebound p))
          ((< player-left field-left)
           (incf (left p) (* 2 (- field-left player-left)))
           (border-collision-consequence
	     p player-left (avg player-top player-bottom) pf)
           (h-rebound p))
          ((>= player-bottom field-bottom)
           (decf (top p) (* 2 (- player-bottom field-bottom)))
           (border-collision-consequence
	     p (avg player-left player-right) player-bottom pf)
           (v-rebound p))
          ((< player-top field-top)
           (incf (top p) (* 2 (- field-top player-top)))
           (border-collision-consequence
	     p (avg player-left player-right) player-top pf)
           (v-rebound p))))
  (values))

(defmethod border-collision-consequence ((p player) x y pf)
  (declare (ignore p))
  (make-sparks x y pf))

(defun preserve-momentum (p1 p2)
  (rotatef (dx p1) (dx p2))
  (rotatef (dy p1) (dy p2)))

(defun overlap (p1 p2)
  "Answers the common rectangle between two players.  If no overlap,
the coordinates of the rectangle returned will be (partially) backwards."
  (let ((left   (max (left p1) (left p2)))
        (top    (max (top p1) (top p2)))
        (right  (min (right p1) (right p2)))
        (bottom (min (bottom p1) (bottom p2))))
    (list left top right bottom)))

(defun valid-p (left top right bottom)
  (and (<= left right) (<= top bottom)))

(defmethod handle-bumps ((p1 player) (p2 player))
  (unless (eq p1 p2)
    (destructuring-bind (left top right bottom)
                        (overlap p1 p2)
      (when (valid-p left top right bottom)
        (preserve-momentum p1 p2)
        (make-sparks (avg left right) (avg top bottom) (playfield-of p1))
        (move-object p1)
        (move-object p2)
)))
  (values))


;;; Sparks are single-pixel, brightly-colored objects which
;;; are generated whenever there's a collision between two
;;; things.  Like players, they have a velocity; however,
;;; they also have a time-to-live aspect.  Sparks fizzle out
;;; after a certain number of game loop iterations.

(defclass spark (player)
  ((color :accessor color)
   (left :initarg :left :accessor left :initform (error "initarg required"))
   (top :initarg :top :accessor top :initform (error "initarg required"))
   (w :reader w :initform 1)
   (h :reader h :initform 1)
   (ttl :accessor ttl)))

(defmethod init ((s spark))
  (setf (color s) (random-color :range 256 :offset 0))
  (setf (dx s) (- (random 15 *random-state*) 7))
  (setf (dy s) (- (random 15 *random-state*) 7))
  (setf (ttl s) (random 60 *random-state*)))

(defmethod paint-with-renderer ((s spark) r)
  (apply #'sdl2:set-render-draw-color (append (list r) (color s)))
  (sdl2:render-draw-point r (left s) (top s))
  (values))

(defun make-sparks (x y pf)
  (dotimes (unused (random 50 *random-state*))
    (push (make-instance 'spark
                         :left (floor x)
                         :top (floor y)
                         :on-playfield pf)
          *sparks*)))

(defmethod border-collision-consequence ((s spark) x y pf)
  ; Sparks do not generate their own sparks upon
  ; collision with a wall or other player.
  (declare (ignore s x y pf)))


;;; General Game Logic that doesn't really fit in anywhere else.


(defun repaint-playfield (renderer)
  "Use painter's algorithm to redraw the game board."
  (loop for object in (append (list *playfield* *player*)
                              *opponents*
                              *sparks*)
        do (paint-with-renderer object renderer))
  (values))


(defun move-objects ()
  "Update the position of all objects with a non-zero
velocity vector."
  (loop for object in (append (list *player*) *opponents* *sparks*)
        do (move-object object))
  (values))


(defun handle-all-collisions ()
  (let* ((vehicles (append (list *player*) *opponents*))
         (everything (append vehicles *sparks*)))
    (loop for object in everything do (border-collisions object))
    (loop for x in vehicles
          do (loop for y in *opponents*
                   do (handle-bumps x y))))
  (values))


(defun reap-sparks ()
  (delete-if #'(lambda (s)
                 (decf (ttl s))
                 (<= (ttl s) 0))
             *sparks*))


(defun game-loop-iteration (renderer)
  (repaint-playfield renderer)
  (move-objects)
  (handle-all-collisions)
  (reap-sparks)
  (values))


(defun initialize-game (window)
  (multiple-value-bind (w h) (sdl2:get-window-size window)
    (setf *playfield* (make-instance 'playfield :w w :h h)))
  (setf *player* (make-instance 'player :on-playfield *playfield*))
  (dotimes (unused 4)
    (push (make-instance 'opponent :on-playfield *playfield*) *opponents*))
  (values))


(defmacro check-direction (ks key axis fn)
  `(when (sdl2:scancode= (sdl2:scancode-value ,ks)
                         (intern (format nil "SCANCODE-~A" (string ,key))
                                 "KEYWORD"))
     (setf (,axis *player*) (,fn (,axis *player*)))))


(defun game ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "Equilibrium/CL" :flags '(:shown))
      (sdl2:with-renderer (renderer window)
        (initialize-game window)
        (sdl2:with-event-loop ()
          (:keydown (:keysym ks)
                    (check-direction ks 'w dy 1-)
                    (check-direction ks 's dy 1+)
                    (check-direction ks 'a dx 1-)
                    (check-direction ks 'd dx 1+))
          (:keyup (:keysym ks)
                  (when (sdl2:scancode= (sdl2:scancode-value ks)
                                        :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (game-loop-iteration renderer)
                 (sdl2:render-present renderer)
                 (sdl2:delay 33))
          (:quit () t)))))
  (values))


#+sbcl (sdl2:make-this-thread-main #'game)
#-sbcl (game)

