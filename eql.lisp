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


(defparameter *playfield* nil)
(defparameter *player* nil)
(defparameter *opponents* nil)


(defgeneric paint-with-renderer (p r))
(defgeneric move-object (p))
(defgeneric handle-collisions (p))


(defclass playfield ()
  ((w :initarg :w :reader w :initform (error ":w required"))
   (h :initarg :h :reader h :initform (error ":h required"))))

(defmethod paint-with-renderer ((p playfield) r)
  (sdl2:set-render-draw-color r 0 0 0 255)
  (sdl2:render-clear r)
  (sdl2:set-render-draw-color r 255 255 255 255)
  (sdl2:render-draw-rect r (sdl2:make-rect 0 0 (w p) (h p)))
  (values))

(defmethod move-object ((p playfield))
  ; playfields are static; we don't bundge.
  (values))

(defmethod handle-collisions ((p playfield))
  ; playfields have nothing they can collide with.
  (values))


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
  ((color :reader color :initform '(0 255 255 255))))

(defmethod initialize-instance :after ((p player) &key)
  (setf (left p) (/ (- (w (playfield-of p)) (w p)) 2))
  (setf (top p)  (/ (- (h (playfield-of p)) (h p)) 2)))

(defmethod initialize-instance :after ((o opponent) &key)
  (setf (dx o) (- (random 7 *random-state*) 3))
  (setf (dy o) (- (random 7 *random-state*) 3))
  (setf (left o) (random (w (playfield-of o)) *random-state*))
  (setf (top o)  (random (h (playfield-of o)) *random-state*)))

(defun right (p)
  (+ (left p) (w p)))

(defun bottom (p)
  (+ (top p) (h p)))

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

(defun h-rebound (p)
  (setf (dx p) (- (dx p))))

(defun v-rebound (p)
  (setf (dy p) (- (dy p))))

(defmethod move-object ((p player))
  (setf (left p) (+ (left p) (dx p)))
  (setf (top p) (+ (top p) (dy p)))
  (values))

(defmethod handle-collisions ((p player))
  ; let-block originally intended to help in debugging an issue.
  ; However, in retrospect, this is better b/c it's faster to run.
  (let ((player-right (right p))
        (player-left (left p))
        (player-top (top p))
        (player-bottom (bottom p))
        (field-left 0)
        (field-right (w (playfield-of p)))
        (field-top 0)
        (field-bottom (h (playfield-of p))))
    (cond ((>= player-right field-right)
           ; We've exceeded the right edge by N pixels.  Moving back
           ; by N pixels puts us right on the edge, which isn't realistic.
           ; Move by another N pixels to properly emulate the reflection
           ; off the wall.
           (decf (left p) (* 2 (- player-right field-right)))
           (h-rebound p))
          ((< player-left field-left)
           (incf (left p) (* 2 (- field-left player-left)))
           (h-rebound p))
          ((>= player-bottom field-bottom)
           (decf (top p) (* 2 (- player-bottom field-bottom)))
           (v-rebound p))
          ((< player-top field-top)
           (incf (top p) (* 2 (- field-top player-top)))
           (v-rebound p))))
  (values))


(defun repaint-playfield (renderer)
  "Use painter's algorithm to redraw the game board."
  (mapcar #'(lambda (obj) (paint-with-renderer obj renderer))
          (append (list *playfield* *player*) *opponents*))
  (values))


(defun move-objects ()
  "Update the position of all objects with a non-zero
velocity vector."
  (mapcar #'move-object (append (list *player*) *opponents*))
  (values))


(defun handle-all-collisions ()
  (mapcar #'handle-collisions (append (list *player*) *opponents*))
  (values))


(defun game-loop-iteration (renderer)
  (repaint-playfield renderer)
  (move-objects)
  (handle-all-collisions)
  (values))


(defun initialize-game (window)
  (multiple-value-bind (w h) (sdl2:get-window-size window)
    (setf *playfield* (make-instance 'playfield :w w :h h)))
  (setf *player* (make-instance 'player :on-playfield *playfield*))
  (setf *opponents*
        (mapcar
          #'(lambda (x) (declare (ignore x))
              (make-instance 'opponent :on-playfield *playfield*))
          '(1 2 3 4)))
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

