#!/usr/local/bin/sbcl --script

(require 'asdf)

(defgeneric upstream-source (type arg)
  (:method (type arg)
    (ecase type
      ((:bzr :cvs :git :http :https :mercurial :svn :darcs) arg))))

(defmacro define-upstream-source (type &optional format-control)
  (let ((arg (gensym "ARG")))
    `(defmethod upstream-source ((type (eql ,type)) ,arg)
       (declare (ignore type))
       (format nil ,(or format-control "~A") ,arg))))

(define-upstream-source :ediware-http "https://github.com/edicl/~A.git")
(define-upstream-source :froydware "http://method-combination.net/lisp/files/~A.tar.gz")
(define-upstream-source :svcware "http://homepage.mac.com/svc/~A/~:*~A.tar.gz")
(define-upstream-source :hungarian-darcs "http://dwim.hu/darcs/~A")
(define-upstream-source :clnet-darcs "http://common-lisp.net/project/~A/darcs/~:*~A")
(define-upstream-source :kmr-git "http://git.kpe.io/~A.git")
(define-upstream-source :wcpware-http)
(define-upstream-source :latest-github-release)
(define-upstream-source :latest-github-tag)
(define-upstream-source :tagged-git)
(define-upstream-source :branched-git)
(define-upstream-source :single-file)

(defun parse-source-txt (source-file)
  (let* ((line (uiop:read-file-line source-file))
         (space-pos (position #\Space line :test #'char=)))
    (list (subseq line 0 space-pos)
          (subseq line
                  (1+ space-pos)
                  (position #\Space line :start (1+ space-pos) :test #'char=)))))

(defun find-release-source-txt (release-name)
  (let ((quicklisp-projects (probe-file #P"quicklisp-projects/")))
    (unless quicklisp-projects
      (error "Directory 'quicklisp-projects/' not found."))
    (let* ((projects-directory (merge-pathnames #P"projects/" quicklisp-projects))
           (projects-directory (or (probe-file projects-directory)
                                   ;; For older versions of quicklisp-projects
                                   quicklisp-projects)))
      (let ((release-dir (uiop:ensure-directory-pathname
                           (merge-pathnames release-name projects-directory))))
        (unless (probe-file release-dir)
          (error "Release '~A' is not found in quicklisp-projects." release-name))
        (let ((source.txt (merge-pathnames #P"source.txt" release-dir)))
          (unless (probe-file source.txt)
            (error "File '~A' not found." source.txt))
          source.txt)))))

(defun main ()
  (destructuring-bind ($0 &optional release-name &rest args)
      sb-ext:*posix-argv*
    (declare (ignore $0 args))
    (let ((source-file (find-release-source-txt release-name)))
      (destructuring-bind (type arg)
          (parse-source-txt source-file)
        (write-line
          (upstream-source (intern (string-upcase type) :keyword) arg))))))

(main)
