(defvar *repertoire* nil)

(defun make-song (artist name rating learned)
  (list :artist artist
        :name name
        :rating rating
        :learned learned
        )
  )

(defun add-song (song) (push song *repertoire*))

(defun dump-repertoire ()
  (dolist (song *repertoire*)
    (format t "~{~a:~10t~a~%~}~%" song)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-song ()
  (make-song
    (prompt-read "Artist")
    (prompt-read "Title")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) 
    (y-or-n-p "Learned? [y/n]")))

(defun add-songs ()
  (loop (add-song (prompt-song))
        (if (not (y-or-n-p "Add another one? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax 
      (print *repertoire* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax 
      (setf *repertoire* (read in)))))

(defun select (where)
  (remove-if-not where *repertoire*))

(defun where (&key artist name rating (learned nil learned-p))
  #'(lambda (song)
      (and
        (if name  (string-equal (getf song :name) name) t)
        (if artist  (string-equal (getf song :artist) artist) t)
        (if rating  (string-equal (getf song :rating) rating) t)
        (if learned-p  (string-equal (getf song :learned) learned) t))))

(defun update (selector-fn &key song artist rating (learned nil learned-p)) 
  (setf *repertoire* (mapcar 
               #'(lambda (row) 
                   (when (funcall selector-fn row)
                     (if song (setf (getf row :song) song))
                     (if artist (setf (getf row :artist) artist))
                     (if rating (setf (getf row :rating) rating))
                     (if learned-p (setf (getf row :learned) learned)))
                   row) *repertoire*)))

(defun delete-rows (selector-fn)
  (setf *repertoire* (remove-if selector-fn *repertoire*)))
