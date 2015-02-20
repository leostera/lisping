;; woo, lets make lists
(list 1 2 3)
;; now with props
(list :a 1 :b 2 :c 3)

;; now what?
;; so getf makes plists a poor man's hash table
;; let's try to save a list to a var
;; this won't work => (let my-list (list :name "Leandro" :age 23))

(getf (list :name "Leandro" :age 23) :age)
(getf (list :name "Leandro" :age 23) :name)
;; kewl!

;; okay so this was supposed to be a simple database
;; let's make it so, we'll store our repertoire here
(defun make-song (artist name rating learned)
  (list :artist artist
        :name name
        :rating rating
        :learned learned
        )
  )

(make-song "Keaton Henson" "You" 50000 "No")
(make-song "Kevin Devine" "Brothers Blood" 5000 "Sorta")
;; this two are effectively usable lists
;; let's si...have I learned Kevin Devine's Brothers Blood yet?
(getf (make-song "Kevin Devine" "Brothers Blood" 5000 "Sorta") :learned)
;; sorta

;; let's define our repertoire
(defvar *repertoire* nil)

;; where we should be able to add songs to!
(defun add-song (song) (push song *repertoire*))

(add-song (make-song "Keaton Henson" "You" 50000 "No"))
(add-song (make-song "Kevin Devine" "Brothers Blood" 5000 "Sorta"))

;; quickly iterate and show all songs
(defun dump-repertoire ()
  (dolist (song *repertoire*)
    (format t "~{~a:~10t~a~%~}~%" song)))

;;  let's ask the user to input a song into our repertoire
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; aweseom, that should be enough to make a add-song song
;; and pass it all our params
(defun prompt-song ()
  (make-song
    (prompt-read "Artist")
    (prompt-read "Title")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) 
    (y-or-n-p "Learned? [y/n]")))

(defun add-songs ()
  (loop (add-song (prompt-song))
        (if (not (y-or-n-p "Add another one? [y/n]: ")) (return))))

; awesome sauce, now we can add multiple songs to our repertoire...
; ...our days to rockstardom are counted!

; but wouldn't it be neat if we could just save this thing
; like just in case we cloesd the interpreter and lost all our shit
; (like happened a few seconds ago)
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    ; with-standard-io-syntax seems to be some sort of 
    ; macro construct that's happening inside of
    ; with-open-file, or thanks to it
    (with-standard-io-syntax 
      (print *repertoire* out))))

; and with each saving comes a loading
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax 
      (setf *repertoire* (read in)))))

; i got bored of writing comments
(defun select (where)
  (remove-if-not where *repertoire*))

(defun where (&key artist name rating (learned nil learned-p))
  #'(lambda (song)
      (and
        (if name  (string-equal (getf song :name) name) t)
        (if artist  (string-equal (getf song :artist) artist) t)
        (if rating  (string-equal (getf song :rating) rating) t)
        (if learned-p  (string-equal (getf song :learned) learned) t)
        )))


