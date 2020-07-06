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


(defparameter *visibles* nil)


(defun reset-playfiend-to-black (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))


(defun repaint-playfield (renderer)
  (reset-playfield-to-black renderer))


(defun game-loop-iteration (renderer)
  (repaint-playfield renderer))


(defun main ()
  (sdl2:with-init ()
    (sdl2:with-window (window :w 768 :h 768 :title "Equilibrium/CL" :flags '(:shown))
      (sdl2:with-renderer (renderer window)
	(sdl2:with-event-loop ()
	  (:keyup (:keysym ks)
		  (when (sdl2:scancode= (sdl2:scancode-value ks) :scancode-escape)
		    (sdl2:push-event :quit)))
	  (:idle ()
		 (game-loop-iteration renderer)
		 (sdl2:render-present renderer)
		 (sdl2:delay 33))
	  (:quit () t))))))

(main)
