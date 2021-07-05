#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))
(load (merge-pathnames #P"../lib/dependencies.lisp" *load-pathname*))

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
                               `(("name" . ,system-name)
                                 ("system_file_name" . ,(pathname-name system-file))
                                 ("required_systems" . ,(remove-duplicates
                                                          (append defsystem-depends-on
                                                                  depends-on
                                                                  weakly-depends-on)
                                                          :from-end t
                                                          :test 'equal)))))
            #()))
    ("systems_metadata_url" . ,(bucket-release-url release "/systems.json"))
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
        ((equal command "systems")
         (dolist (system (ql-dist:provided-systems release))
           (write-line (ql-dist:name system))))
        (t
         (error "Unexpected subcommand: '~A'" command))))))

(main)
