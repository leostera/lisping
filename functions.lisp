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
