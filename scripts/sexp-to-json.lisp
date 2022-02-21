#!/usr/local/bin/sbcl --script

(require 'asdf)

(dolist (dir (uiop:subdirectories
               (merge-pathnames #P"deps/" *default-pathname-defaults*)))
  (push dir asdf:*central-registry*))

(let ((*standard-output* (make-broadcast-stream))
      (stderr *error-output*)
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((error
                   (lambda (e)
                     (uiop:print-condition-backtrace e :stream stderr))))
    (asdf:load-system :cl-json)))

(defun main ()
  (let ((form (read)))
    (cl-json:encode-json form)
    (fresh-line)))

(main)
