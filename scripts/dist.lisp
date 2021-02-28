#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

(defun dist-info (dist)
  `(("name" . ,(ql-dist:name dist))
    ("version" . ,(ql-dist:version dist))
    ("system_index_url" . ,(ql-dist:system-index-url dist))
    ("release_index_url" . ,(ql-dist:release-index-url dist))
    ("distinfo_subscription_url" . ,(ql-dist::distinfo-subscription-url dist))))

(defun main ()
  (destructuring-bind ($0 &optional (name "quicklisp") command &rest args)
      sb-ext:*posix-argv*
    (declare (ignore $0 args))
    (let ((dist (ql-dist:find-dist name)))
      (unless dist
        (error "No dist named '~A' found" name))
      (cond
        ((null command)
         (format t "~&~S~%" (dist-info dist)))
        ((equal command "releases")
         (dolist (release (ql-dist:provided-releases dist))
           (write-line (ql-dist:name release))))
        ((equal command "version")
         (write-line (cdr (assoc "version" (dist-info dist) :test 'equal))))
        (t
         (error "Unexpected subcommand: '~A'" command))))))

(main)
