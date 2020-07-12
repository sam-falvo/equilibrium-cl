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


(defgeneric paint-with-renderer (p r))
(defgeneric move-object (p))


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


(defclass player ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (w :initform 16 :reader w)
   (h :initform 16 :reader h)
   (dx :accessor dx :initform 0)
   (dy :accessor dy :initform 0)
   (pf :initarg :on-playfield :initform nil :reader playfield-of)))

(defmethod initialize-instance :after ((p player) &key)
  (setf (x p) (/ (w (playfield-of p)) 2))
  (setf (y p) (/ (h (playfield-of p)) 2)))

(defmethod paint-with-renderer ((p player) r)
  (let* ((width (w p))
         (height (h p))
         (left (- (x p) (/ width 2)))
         (top (- (y p) (/ height 2)))
         (right (+ (x p) (/ width 2)))
         (bottom (+ (y p) (/ height 2))))
    (sdl2:set-render-draw-color r 255 255 255 255)
    (sdl2:render-draw-line r left top right top)
    (sdl2:render-draw-line r right top right bottom)
    (sdl2:render-draw-line r right bottom left bottom)
    (sdl2:render-draw-line r left bottom left top)
    (values)))

(defmethod move-object ((p player))
  (setf (x p) (+ (x p) (dx p)))
  (setf (y p) (+ (y p) (dy p)))
  (values))


(defun repaint-playfield (renderer)
  "Use painter's algorithm to redraw the game board."
  (mapcar #'(lambda (obj) (paint-with-renderer obj renderer))
          (list *playfield* *player*))
  (values))


(defun move-objects ()
  "Update the position of all objects with a non-zero
velocity vector."
  (mapcar #'move-object (list *player*)))


(defun game-loop-iteration (renderer)
  (repaint-playfield renderer)
  (move-objects)
  (values))


(defun initialize-game (window)
  (multiple-value-bind (w h) (sdl2:get-window-size window)
    (setf *playfield* (make-instance 'playfield :w w :h h)))
  (setf *player* (make-instance 'player :on-playfield *playfield*))
  (values))


(defmacro check-direction (ks key axis fn)
  `(when (sdl2:scancode= (sdl2:scancode-value ,ks)
                         (intern (format nil "SCANCODE-~A" (string ,key)) "KEYWORD"))
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
                  (when (sdl2:scancode= (sdl2:scancode-value ks) :scancode-escape)
                    (sdl2:push-event :quit)))
          (:idle ()
                 (game-loop-iteration renderer)
                 (sdl2:render-present renderer)
                 (sdl2:delay 33))
          (:quit () t)))))
  (values))


#+sbcl (sdl2:make-this-thread-main #'game)
#-sbcl (game)

