(defpackage #:dist-extractor/lib/dependencies
  (:use #:cl
        #:dist-extractor/lib/asdf-types)
  (:export #:project-dependencies))
(in-package #:dist-extractor/lib/dependencies)

(defparameter *exclude-directories*
  asdf::*default-source-registry-exclusions*)

(defun sbcl-contrib-p (name)
  (let ((name (princ-to-string name)))
    (and (<= 3 (length name))
         (string-equal name "sb-" :end1 3))))

(defun directory-system-files (directory)
  (labels ((asd-file-p (path)
             (and (equal (pathname-type path) "asd")
                  ;; KLUDGE: Ignore skeleton.asd of CL-Project
                  (not (search "skeleton" (pathname-name path)))))
           (collect-asd-files-in-directory (dir)
             (let ((dir-name (car (last (pathname-directory dir)))))
               (unless (or (find dir-name
                                 *exclude-directories*
                                 :test #'string=)
                           (and (stringp dir-name)
                                (char= (aref dir-name 0) #\.)))
                 (nconc
                   (mapcar #'truename
                           (remove-if-not #'asd-file-p
                                          (uiop:directory-files dir)))
                   (mapcan #'collect-asd-files-in-directory (uiop:subdirectories dir)))))))
    (sort
      (collect-asd-files-in-directory directory)
      #'string<
      :key #'pathname-name)))

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
         (push (list system-name
                     (depends-on defsystem-depends-on asd-file)
                     (depends-on depends-on asd-file)
                     (depends-on weakly-depends-on asd-file))
               (gethash asd-file *registry*)))))
    ((macro-function (first form))
     (read-asd-form (macroexpand-1 form) asd-file))))

(defun make-hook (old-hook asd-file)
  (lambda (fun form env)
    (when (and (consp form)
               (eq (first form) 'asdf:defsystem)
               (and (equal *load-pathname* asd-file)))
      (read-asd-form form asd-file))
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

(defun project-dependencies (directory)
  (let ((*registry* (make-hash-table :test 'equal)))
    (dolist (system-file (directory-system-files directory))
      (handler-bind ((error
                       (lambda (e)
                         (uiop:print-condition-backtrace e)
                         (let ((restart (find-restart 'abort e)))
                           (when restart
                             (invoke-restart restart))))))
        (with-autoload-on-missing
          (let ((*macroexpand-hook* (make-hook *macroexpand-hook* system-file)))
            (asdf:load-asd system-file)))))
    (mapcar (lambda (system-file)
              (let ((value (gethash system-file *registry*)))
                (cons system-file (nreverse value))))
            (sort (hash-keys *registry*)
                  #'string<
                  :key #'pathname-name))))
