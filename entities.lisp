(defpackage :equilibrium-entities
  (:export make-entity)
  (:nicknames :eqe)
  (:use cl))
(in-package :equilibrium-entities)


(defparameter *entity-counter* 0
  "Contains the ID of the next entity to create.  Entity IDs are not reused.")


(defun make-entity ()
  "Allocates a new entity ID."
  (incf *entity-counter*)
  *entity-counter*)


