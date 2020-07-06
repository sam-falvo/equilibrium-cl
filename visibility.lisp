(defpackage :equilibrium-visibility
  (:export hide-everything
	   visible
	   is-visible-p
	   draw-everything
	   *viewables*)
  (:nicknames :eqv)
  (:use cl))
(in-package :equilibrium-visibility)


(defparameter *viewables* (make-hash-table)
  "Contains a set of all currently visible objects on the screen.")


(defun hide-everything ()
  "Hides everything; nothing will be visible on the screen."

  (setf *viewables* (make-hash-table)))


(defun visible (ent draw-fn)
  "Makes an entity ENT visible by registering its draw function DRAW-FN.
You may call (VISIBLE ent another-draw-fn) to change the DRAW-FN at
any time, provided it is not NIL.

The DRAW-FN is called with (FUNCALL DRAW-FN ent), where ENT is the same
entity ID as that provided for this function."

  (unless draw-fn (error "draw-fn parameter cannot be NIL"))
  (when draw-fn (setf (gethash ent *viewables*) draw-fn)))


(defun is-visible-p (ent)
  "Answers T if the entity ENT is known to be visible; NIL otherwise."

  (not (null (gethash ent *viewables*))))


(defun draw-everything ()
  "Draws all currently visible entities."

  (maphash #'(lambda (k v) (format t "~A:~A~%" k v) (funcall v k)) *viewables*))

