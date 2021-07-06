#!/usr/local/bin/sbcl --script

(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

(defun readme-file-p (path)
  (check-type path pathname)
  (equal (pathname-name path) "README"))

(defun file-encoding (file)
  (string-right-trim
    '(#\Newline)
    (uiop:run-program
      `("file" "-b" "--mime-encoding" ,(uiop:native-namestring file))
      :output :string)))

(defun supported-encoding-p (encoding)
  (when (stringp encoding)
    (and
      (uiop:run-program
        (format nil "iconv -l | grep -i '^~A//$'"
                encoding)
        :ignore-error-status t
        :output :string)
      t)))

(defun file-size (file)
  (with-open-file (in file)
    (file-length in)))

(defun read-file-in-utf-8 (file)
  (let ((size (file-size file)))
    (when (= size 0)
      (return-from read-file-in-utf-8 "")))

  (let ((encoding (file-encoding file)))
    (uiop:run-program `("iconv"
                        ,@(when (supported-encoding-p encoding)
                            `("-f" ,encoding))
                        "-t" "utf-8"
                        "-c"
                        ,(uiop:native-namestring file))
                      :ignore-error-status t
                      :output :string
                      :error-output *error-output*)))

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
                              ("content" . ,(read-file-in-utf-8 file))))
                          (remove-if-not
                            #'readme-file-p
                            (uiop:directory-files release-dir)))
                        #()))))))))

(main)
