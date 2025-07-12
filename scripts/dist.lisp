#!/usr/local/bin/sbcl --script

(require 'asdf)
(load (merge-pathnames #P".roswell/lisp/quicklisp/setup.lisp" (user-homedir-pathname)))

(defun bucket-dist-url (dist path)
  (format nil "~A/~A/~A~A"
          (or (uiop:getenv "BUCKET_BASE_URL") "")
          (ql-dist:name dist)
          (ql-dist:version dist)
          path))

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

(defun dist-info (dist)
  `(("name" . ,(ql-dist:name dist))
    ("version" . ,(ql-dist:version dist))
    ("system_index_url" . ,(ql-dist:system-index-url dist))
    ("release_index_url" . ,(ql-dist:release-index-url dist))
    ("archive_base_url" . ,(ql-dist::archive-base-url dist))
    ("distinfo_subscription_url" . ,(ql-dist::distinfo-subscription-url dist))
    ("canonical_distinfo_url" . ,(ql-dist:canonical-distinfo-url dist))
    ("provided_releases_count" . ,(length (ql-dist:provided-releases dist)))
    ("provided_releases_url" . ,(bucket-dist-url dist "/releases.json"))
    ("extract_errors_url" . ,(bucket-dist-url dist "/errors.log"))))

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
         (format t "~&~S~%"
                 (mapcar (lambda (release)
                           (cons (ql-dist:name release)
                                 (bucket-release-url release "/info.json")))
                         (ql-dist:provided-releases dist))))
        ((equal command "version")
         (write-line (cdr (assoc "version" (dist-info dist) :test 'equal))))
        (t
         (error "Unexpected subcommand: '~A'" command))))))

(main)
