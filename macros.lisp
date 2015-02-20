(defmacro backwards (expr) (reverse expr))

; doesn't work!
; (defun make-comparison-expr  (field value)
  ; (list equal (list getf cd field) value))

; we need to semi-quote the stuff that is not a variable
; in this case: equal, getf, and cd
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

; or even better
; this form just evaluates what is comma-preceded
; the rest is not evaluated
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

; this function takes a list of fields 
; and returns a list of comparison expressions
; remember fields should be a list
; as in (list :artist "The Who")
; and not (:artist "The Who")
(defun make-comparison-list (fields) 
  (loop while fields 
        collecting (make-comparison-expr (pop fields) (pop fields))))

; macro time.
(defmacro where (&rest clauses)
  `#'(lambda (song) (and ,@(make-comparison-list clauses))))
