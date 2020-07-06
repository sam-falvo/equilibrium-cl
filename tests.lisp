(load "entities")
(load "visibility")


(defpackage :equilibrium-cl-tests
  (:use :cl))
(in-package :equilibrium-cl-tests)


;;;;;; CORE TEST FUNCTIONALITY

(defun expand-test (name actual-expr fn expected-expr setup-form teardown-form vars-form)
  `(progn
     (defun ,name ()
       (unwind-protect
         (let ,vars-form
           (progn
             ,setup-form
             (let ((expected ,expected-expr)
                   (actual ,actual-expr))
               (unless (,fn actual expected)
                 (error "~A: Expected ~A; got ~A" (string ',name) expected actual)))))
         ,teardown-form))
     (,name)))


(defmacro test (name expected-expr fn actual-expr &key setup teardown vars)
  (expand-test name expected-expr fn actual-expr setup teardown vars))


(defun not-eql (x y) (not (eql x y)))


;;;;;; ENTITIES


(test entities-must-have-unique-ids
      (eqe:make-entity) not-eql (eqe:make-entity))


;;;;;; VISIBILITY


(defun noop (e) (declare (ignore e)))


(test marking-entity-visible
      (eqv:is-visible-p ent) eql t
      :setup (progn (eqv:hide-everything) (eqv:visible ent #'noop))
      :vars ((ent (eqe:make-entity))))


(test visible-entities-can-be-redrawn
      callback-was-called eql t
      :setup (progn
	       (eqv:hide-everything)
	       (eqv:visible
		 ent
		 #'(lambda (e) (declare (ignore e)) (setf callback-was-called t)))
	       (eqv:draw-everything))
      :vars ((ent (eqe:make-entity))
	     (callback-was-called)))


;;;;;; All done.  If no diagnostic output, then everything passed.


(sb-ext:quit)

