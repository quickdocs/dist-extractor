#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))
(load (merge-pathnames #P"../lib/asdf-types.lisp" *load-pathname*))
(load (merge-pathnames #P"../lib/dependencies.lisp" *load-pathname*))

(do-external-symbols (symb :dist-extractor/lib/asdf-types)
  (import symb))
(do-external-symbols (symb :dist-extractor/lib/dependencies)
  (import symb))

(defun release-last-updated-version (release)
  (let* ((dist (ql-dist:dist release))
         (prefix-len (length (format nil "~Aarchive/~A/" (ql-dist::archive-base-url dist) (ql-dist:name release)))))
    (subseq (ql-dist:archive-url release)
            prefix-len
            (+ 10 prefix-len))))

(defun bucket-release-url (release path)
  (format nil "~A/~A/~A/releases/~A~A"
          (or (uiop:getenv "BUCKET_BASE_URL") "")
          (ql-dist:name (ql-dist:dist release))
          (release-last-updated-version release)
          (ql-dist:prefix release)
          path))

(defun uniq (list)
  (remove-duplicates list
                     :key (lambda (v) (cdr (assoc "name" v :test 'equal)))
                     :test 'equal
                     :from-end t))

(defun asdf-system-metadata (system)
  (let ((system-dir (asdf:system-source-directory system)))
    `(("name" . ,(name (asdf:component-name system)))
      ("class" . ,(system-class-name system))
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
      ("source_control" . ,(source-control (asdf:system-source-control system))))))

(defun release-info (release)
  `(("project_name" . ,(ql-dist:project-name release))
    ("archive_url" . ,(ql-dist:archive-url release))
    ("archive_size" . ,(ql-dist:archive-size release))
    ("archive_content_sha1" . ,(ql-dist:archive-content-sha1 release))
    ("prefix" . ,(ql-dist:prefix release))
    ("systems"
     . ,(or (loop for (system-file . system-defs) in (project-dependencies (ql-dist:base-directory release))
                  append (loop for (system-name defsystem-depends-on depends-on weakly-depends-on) in system-defs
                               collect
                               (let ((system (let ((*standard-output* (make-broadcast-stream)))
                                               (handler-bind ((warning #'muffle-warning))
                                                 (asdf:find-system system-name)))))
                                 (append
                                   `(("name" . ,system-name)
                                     ("system_file_name" . ,(pathname-name system-file)))
                                   (asdf-system-metadata system)
                                   `(("defsystem_depends_on" . ,(or (uniq defsystem-depends-on) #()))
                                     ("depends_on" . ,(or (uniq depends-on) #()))
                                     ("weakly_depends_on" . ,(or (uniq weakly-depends-on) #())))))))
            #()))
    ("readme_url" . ,(bucket-release-url release "/readme.json"))))

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
        (t
         (error "Unexpected subcommand: '~A'" command))))))

(main)
