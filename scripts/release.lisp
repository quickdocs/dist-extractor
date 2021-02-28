#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

(defun release-info (release)
  `(("project_name" . ,(ql-dist:project-name release))
    ("archive_url" . ,(ql-dist:archive-url release))
    ("archive_size" . ,(ql-dist:archive-size release))
    ("archive_content_sha1" . ,(ql-dist:archive-content-sha1 release))
    ("prefix" . ,(ql-dist:prefix release))
    ("systems" . ,(mapcar (lambda (system)
                            `(("name" . ,(ql-dist:name system))
                              ("system_file_name" . ,(ql-dist:system-file-name system))
                              ("required_systems" . ,(ql-dist:required-systems system))))
                          (ql-dist:provided-systems release)))))

(defun main ()
  (destructuring-bind ($0 &optional name command &rest args)
      sb-ext:*posix-argv*
    (declare (ignore $0))
    (when (null name)
      (error "At least one argument is required"))
    (let ((release (ql-dist:find-release name)))
      (unless release
        (error "No release named '~A' found" name))
      (cond
        ((null command)
         (format t "~&~S~%" (release-info release)))
        ((equal command "systems")
         (dolist (system (ql-dist:provided-systems release))
           (write-line (ql-dist:name system))))
        (t
         (error "Unexpected subcommand: '~A'" command))))))

(main)
