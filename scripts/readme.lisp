#!/usr/local/bin/sbcl --script

(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

(defun readme-file-p (path)
  (check-type path pathname)
  (equal (pathname-name path) "README"))

(defun main ()
  (destructuring-bind ($0 &optional name &rest args)
      sb-ext:*posix-argv*
    (declare (ignore $0 args))
    (when (null name)
      (error "At least one argument is required"))
    (let ((release (ql-dist:find-release name)))
      (unless release
        (error "No release named '~A' found" name))
      (let ((release-dir (ql-dist:base-directory release)))
        (format t "~&~S~%"
                `(("name" . ,name)
                  ("readme_files" .
                   ,(or (mapcar
                          (lambda (file)
                            `(("filename" . ,(file-namestring file))
                              ("content" . ,(uiop:read-file-string file))))
                          (remove-if-not
                            #'readme-file-p
                            (uiop:directory-files release-dir)))
                        #()))))))))

(main)
