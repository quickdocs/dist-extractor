#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload '(:yason :alexandria) :silent t)

(defun alistp (value)
  (and (consp value)
       (consp (first value))
       (stringp (car (first value)))))

(defun main ()
  (let ((form (read)))
    (labels ((convert (v)
               (cond
                 ((alistp v)
                  (alexandria:alist-hash-table
                    (loop for (k . v) in v
                          collect (cons k (convert v)))))
                 ((consp v)
                  (map 'vector #'convert v))
                 ((null v)
                  nil)
                 ((symbolp v)
                  (string-downcase v))
                 (t v))))
      (yason:encode (convert form)))
    (fresh-line)))

(main)
