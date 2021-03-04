#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

(defun normalize-dependency (name &optional system-directory)
  (labels ((component-name (value)
             (etypecase value
               (string value)
               (symbol (string-downcase value))))
           (feature-expression (value)
             (etypecase value
               (symbol (string-downcase value))
               (cons
                 (list (string-downcase (first value))
                       (feature-expression (second value))))))
           (version-specifier (value)
             (etypecase value
               (string value)
               (symbol (string-downcase value))
               (cons (ecase (first value)
                       (:read-file-form (apply #'uiop:read-file-form
                                               (merge-pathnames (second value) system-directory)
                                               (rest (rest value))))
                       (:read-file-line (apply #'uiop:read-file-line
                                               (merge-pathnames (second value) system-directory)
                                               (rest (rest value))))))))
           (dependency-def (value)
             (typecase value
               (cons (ecase (first value)
                       (:feature (append (dependency-def (third value))
                                         `(("feature" . ,(feature-expression (second value))))))
                       (:version `(("name" . ,(component-name (second value)))
                                   ("version" . ,(version-specifier (third value)))))
                       (:require `(("name" . ,(component-name (second value)))))))
               (otherwise `(("name" . ,(component-name value)))))))
    (dependency-def name)))

(defun asdf-system-metadata (system-name)
  (let* ((system (let ((*standard-output* (make-broadcast-stream)))
                   (handler-bind ((warning #'muffle-warning))
                     (asdf:find-system system-name))))
         (system-dir (asdf:system-source-directory system)))
    (flet ((dep (v)
             (normalize-dependency v system-dir)))
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
        ("defsystem_depends_on" . ,(mapcar #'dep (asdf:system-defsystem-depends-on system)))
        ("depends_on" . ,(mapcar #'dep (asdf:system-depends-on system)))
        ("weakly_depends_on" . ,(mapcar #'dep (asdf:system-weakly-depends-on system)))))))

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
