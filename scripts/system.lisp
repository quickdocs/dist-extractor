#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

(defun asdf-system-metadata (system-name)
  (let ((system (let ((*standard-output* (make-broadcast-stream)))
                  (asdf:find-system system-name))))
    `(("name" . ,(asdf:component-name system))
      ("long_name" . ,(asdf:system-long-name system))
      ("version" . ,(asdf:component-version system))
      ("description" . ,(asdf:system-description system))
      ("long_description" . ,(asdf:system-long-description system))
      ("author" . ,(asdf:system-author system))
      ("maintainer" . ,(asdf:system-maintainer system))
      ("mailto" . ,(asdf:system-mailto system))
      ("license" . ,(asdf:system-license system))
      ("homepage" . ,(asdf:system-homepage system))
      ("bug_tracker" . ,(asdf:system-bug-tracker system))
      ("source_control" . ,(asdf:system-source-control system))
      ("defsystem_depends_on" . ,(asdf:system-defsystem-depends-on system))
      ("depends_on" . ,(asdf:system-depends-on system))
      ("weakly_depends_on" . ,(asdf:system-weakly-depends-on system)))))

(defun system-info (system)
  `(("name" . ,(ql-dist:name system))
    ("system_file_name" . ,(ql-dist:system-file-name system))
    ("required_systems" . ,(ql-dist:required-systems system))
    ("metadata" . ,(block nil
                     (handler-bind ((error
                                      (lambda (e)
                                        (uiop:print-condition-backtrace e)
                                        (return nil))))
                       (asdf-system-metadata (ql-dist:name system)))))))

(defun main ()
  (destructuring-bind ($0 &optional name &rest args)
      sb-ext:*posix-argv*
    (declare (ignore $0 args))
    (when (null name)
      (error "No system name is given."))
    (let ((system (ql-dist:find-system name)))
      (unless system
        (error "No system named '~A' found" (ql-dist:name system)))
      (format t "~&~S~%" (system-info system)))))

(main)
