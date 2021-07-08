(defpackage #:dist-extractor/lib/dependencies
  (:use #:cl
        #:dist-extractor/lib/asdf-types)
  (:export #:project-dependencies
           #:directory-lisp-files
           #:lisp-file-system-name
           #:lisp-file-dependencies))
(in-package #:dist-extractor/lib/dependencies)

(defparameter *exclude-directories*
  asdf::*default-source-registry-exclusions*)

(defun sbcl-contrib-p (name)
  (let ((name (princ-to-string name)))
    (and (<= 3 (length name))
         (string-equal name "sb-" :end1 3))))

(defvar *registry*)

(defun read-asd-form (form asd-file)
  (cond
    ((not (consp form)) nil)
    ((eq (first form) 'asdf:defsystem)
     (destructuring-bind (system-name &rest system-form) (cdr form)
       (let ((defsystem-depends-on (getf system-form :defsystem-depends-on))
             (depends-on (getf system-form :depends-on))
             (weakly-depends-on (getf system-form :weakly-depends-on))
             (system-name (asdf::coerce-name system-name)))
         (let ((*standard-output* (make-broadcast-stream)))
           (mapc #'asdf:load-system
                 (mapcar #'dependency-name defsystem-depends-on)))
         (push (list system-name
                     (depends-on defsystem-depends-on (uiop:pathname-directory-pathname asd-file))
                     (depends-on depends-on (uiop:pathname-directory-pathname asd-file))
                     (depends-on weakly-depends-on (uiop:pathname-directory-pathname asd-file)))
               (gethash asd-file *registry*)))))
    ((macro-function (first form))
     (read-asd-form (macroexpand-1 form) asd-file))))

(defvar *in-defsystem* nil)
(defun make-hook (old-hook asd-file)
  (lambda (fun form env)
    (when (and (consp form)
               (eq (first form) 'asdf:defsystem)
               (not *in-defsystem*))
      (let ((*in-defsystem* t))
        (read-asd-form form asd-file)))
    (funcall old-hook fun form env)))

(defun hash-keys (hash)
  (check-type hash hash-table)
  (loop for key being the hash-keys of hash
        collect key))

(defmacro with-autoload-on-missing (&body body)
  (let ((retrying (gensym))
        (package (gensym))
        (e (gensym)))
    `(let ((,retrying (make-hash-table :test 'equal)))
       (#+asdf3.3 asdf/session:with-asdf-session #+asdf3.3 (:override t)
        #-asdf3.3 progn
         (handler-bind ((asdf:missing-component
                          (lambda (,e)
                            (unless (gethash (asdf::missing-requires ,e) ,retrying)
                              (setf (gethash (asdf::missing-requires ,e) ,retrying) t)
                              (ql:quickload (asdf::missing-requires ,e)
                                            :silent t)
                              (invoke-restart (find-restart 'asdf:retry ,e)))))
                        (asdf:load-system-definition-error
                          (lambda (,e)
                            (when (ignore-errors (slot-boundp (asdf/find-system:error-condition ,e) 'sb-c::condition))
                              (let ((,package (slot-value (slot-value (asdf/find-system:error-condition ,e) 'sb-c::condition) 'package)))
                                (when (and (stringp ,package)
                                           (not (gethash ,package ,retrying)))
                                  (setf (gethash ,package ,retrying) t)
                                  (ql:quickload ,package :silent t)
                                  (invoke-restart (find-restart 'asdf:retry ,e))))))))
           ,@body)))))

(defun project-dependencies (release)
  (check-type release ql-dist:release)
  (let ((*registry* (make-hash-table :test 'equal))
        (base-dir (ql-dist:base-directory release))
        (system-files (ql-dist:system-files release)))
    (dolist (system-file (mapcar (lambda (file)
                                   (merge-pathnames file base-dir))
                                 system-files))
      (handler-bind ((error
                       (lambda (e)
                         (uiop:print-condition-backtrace e)
                         (let ((restart (find-restart 'abort e)))
                           (when restart
                             (invoke-restart restart))))))
        (with-autoload-on-missing
          (let ((*macroexpand-hook* (make-hook *macroexpand-hook* system-file)))
            (handler-bind ((warning #'muffle-warning))
              (let ((*standard-output* (make-broadcast-stream))
                    (asdf:*system-definition-search-functions*
                      (remove 'asdf/system-registry:sysdef-source-registry-search
                              asdf:*system-definition-search-functions*)))
                (asdf:load-asd system-file)))))))
    (mapcar (lambda (system-file)
              (let ((value (gethash system-file *registry*)))
                (cons system-file (nreverse value))))
            (sort (hash-keys *registry*)
                  #'string<
                  :key #'pathname-name))))

(defun directory-lisp-files (directory)
  (append (uiop:directory-files directory "*.lisp")
          (loop for subdir in (uiop:subdirectories directory)
                for dir-name = (car (last (pathname-directory subdir)))
                unless (or (find dir-name
                                 *exclude-directories*
                                 :test 'string=)
                           (char= (aref dir-name 0) #\.))
                append (directory-lisp-files subdir))))

(defun starts-with (prefix value)
  (check-type prefix string)
  (check-type value string)
  (and (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

(defun lisp-file-system-name (file root primary-system-name)
  (block nil
    (handler-bind ((error
                     (lambda (e)
                       (declare (ignorable e))
                       ;;(uiop:print-condition-backtrace e)
                       (return nil))))
      (flet ((primary-system-prefix-p (package-name)
               (check-type package-name string)
               (starts-with (format nil "~A/" primary-system-name) package-name)))
        (let ((defpackage-form (asdf/package-inferred-system::file-defpackage-form file)))
          (when defpackage-form
            (let* ((package-names (mapcar #'string-downcase
                                          (cons (second defpackage-form)
                                                (cdr
                                                  (assoc :nicknames (cddr defpackage-form)
                                                         :test #'string=)))))
                   (system-name (find-if
                                  (lambda (name)
                                    (and (primary-system-prefix-p name)
                                         (uiop:pathname-equal
                                           (merge-pathnames
                                             (format nil "~A~@[.~A~]"
                                                     (subseq name (1+ (length primary-system-name)))
                                                     (pathname-type file))
                                             root)
                                           file)))
                                  package-names)))
              (when system-name
                (string-downcase system-name)))))))))

(defun lisp-file-dependencies (file)
  (asdf/package-inferred-system::package-inferred-system-file-dependencies file))
