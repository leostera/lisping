;;;; okay, let's learn a bit about functions today.

;; defining new functions
;; normally functions are defined using the defun macro, like this

; (defun function-name (params*)
;   "Optional doc-string"
;   body-form*)

; sample hello world
(defun hello-world () (format t "hello, world"))

; more complex sum fun
(defun verbose-sum (x y)
  "Sums two numbers after being verbose about it"
  (format t "Woah, we're about to sum ~d and ~d ~%" x y)
  (+ x y))

;; given parameters for a function like verbose-sum
;; these are *required parameters*
;; which means that if you call the function
;; with less or more params than it asks
;; Lisp will signal an error
;;
;; (verbose-sum 2 3) -> Ok
;; (verbose-sum 2) -> Error
;; (verbose-sum 2 3 4) -> Error

;; optional params are defined after &optional
;; as follows

(defun foo-optional-params (a b &optional c d) (list a b c d))

;; and to define default values for optional params
;; you set them as a list with a name and a value

(defun foo-default-params (a &optional (b 12)) (list a b))

;; if you need to know whether the parameter has been passed
;; you can do so by adding a third element to the name value list

(defun foo-supplied-param (a &optional (b 12 b-supplied-p))
  (list a b b-supplied-p))

;; this oughtta tell you whether the param was passed in or not
;; because the user might pass in the exact default value

;; in other cases it can be used to compute a param's value
;; because you know the user didn't pass it in
;; and the default value needs to be computed
;; in some others, it will be enough to reference another param
;; previously defined in the params list
;; as follows

(defun foo-linked-params (width &optional (height width height-supplied-p))
  (list width height height-supplied-p))

;;;; rest parameters!
;;;; nothing to do with RESTfulness

(defun sum (&rest numbers)
  (apply #'+ numbers))

;; this way, calling (sum 1 2 3 4.......10 11 12)
;; would take all the numbers => (1 2 3 4....10 11 12)
;; and call the lisp sum function (+) passing the list
;; as arguments.
;; so (sum 1 2 3) <===> (+ 1 2 3)

;;;; keyword parameters!
;;;; (yay! named params)

(defun foo-key-params (&key (a 0) (b 12 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

;; or you can define other names for the params
(defun foo-named-params (&key ((:apple a) 0) ((:bananas b) 12 b-supplied-p) ((:charles c) (+ a b)))
  (list a b c b-supplied-p))
;; this way you can call
;; (foo-named params :apple 10)
;; which is way more descriptive

;;;; COMBINE THEM!
;;;; feeling like an params alchemist

;;; Notes:
;;;     never mix &optional and &key. weird behavior.
;;;     &rest will render even &key parameters into it

(defun foo-rest-and-key (&rest rest &key a b c)
  (list rest a b c))

;; CL-USER> (foo-rest-and-key :a 1 :b 2 :c 3)
;; ((:A 1 :B 2 :C 3) 1 2 3)
;; see how rest became (:A 1 :B 2 :C 3) without affecting
;; the &key params?
;;;; Returning values from functions ;; example: 
(defun foo-return (n) 
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo-return (list i j))))))

;; we can see (return-from) being used here
;; it requires a block name (in this case, foo-return)
;; and a value
;; what's going on here is that the defun macro
;; wraps the function body in a BLOCK (which is a special op)
;; and names it after the function
;; this way, return-from can return from the BLOCK as long
;; as it's got the name

;; there's a RETURN macro that's syntatic sugar for 
;; RETURN-FROM NIL value
;; which means return from an anonymous block

;;;;  H  I  G  H  E  R
;;;;     O R D E R
;;;; F U N C T I O N S

;; fancy my ascii titles?

;; you can get functions with the FUNCTION function
;; or with #', as follows

(FUNCTION foo-return)
;; is equivalent to
#'foo-return

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

;; 519 CL-USER> (plot #'exp 0 4 1/2)
;; 520 *
;; 521 **
;; 522 ***
;; 523 *****
;; 524 ********
;; 525 *************
;; 526 *********************
;; 527 **********************************
;; 528 *******************************************************
;; 529 NIL

;;;; LAMBDAS
;; lambdas are functions which names are exactly what they do
;; defined as:
((lambda (x y) (+ x y)) 2 3) ; => 5

(plot #'(lambda (x) (* 2 (* x x))) 0 6 1/3)
;; lambda here does the trick instead of defining a named function
;; they can also be returned and whatnot
;; which is pretty rad!

;; okay, onto the next chapter.
