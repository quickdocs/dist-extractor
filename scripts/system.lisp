#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))
(load (merge-pathnames #P"../lib/asdf-types.lisp" *load-pathname*))

(do-external-symbols (symb :dist-extractor/lib/asdf-types)
  (import symb))

(defun asdf-system-metadata (system-name)
  (let* ((system (let ((*standard-output* (make-broadcast-stream)))
                   (handler-bind ((warning #'muffle-warning))
                     (asdf:find-system system-name))))
         (system-dir (asdf:system-source-directory system)))
    `(("name" . ,(name (asdf:component-name system)))
      ("long_name" . ,(long-name (asdf:system-long-name system)))
      ("version" . ,(version (asdf:component-version system) system-dir))
      ("description" . ,(description (asdf:system-description system)))
      ("long_description" . ,(long-description (asdf:system-long-description system)))
      ("authors" . ,(or (authors (asdf:system-author system)) #()))
      ("maintainers" . ,(or (maintainers (asdf:system-maintainer system)) #()))
      ("mailto" . ,(mailto (asdf:system-mailto system)))
      ("license" . ,(license (asdf:system-license system)))
      ("homepage" . ,(homepage (asdf:system-homepage system)))
      ("bug_tracker" . ,(bug-tracker (asdf:system-bug-tracker system)))
      ("source_control" . ,(source-control (asdf:system-source-control system)))
      ("defsystem_depends_on" . ,(or (depends-on (asdf:system-defsystem-depends-on system) system-dir) #()))
      ("depends_on" . ,(or (depends-on (asdf:system-depends-on system) system-dir) #()))
      ("weakly_depends_on" . ,(or (depends-on (asdf:system-weakly-depends-on system) system-dir) #())))))

(defun system-info (system)
  `(("name" . ,(ql-dist:name system))
    ("system_file_name" . ,(ql-dist:system-file-name system))
    ("required_systems" . ,(or (ql-dist:required-systems system) #()))
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
