(in-package #:cl-user)
(defpackage #:dist-extractor/lib/asdf-types
  (:use #:cl)
  (:export #:name
           #:long-name
           #:version
           #:description
           #:long-description
           #:authors
           #:maintainers
           #:mailto
           #:license
           #:homepage
           #:bug-tracker
           #:source-control
           #:depends-on))
(in-package #:dist-extractor/lib/asdf-types)

;;
;; Basic types

(defun lower-string-p (value)
  (and (stringp value)
       (equal (string-downcase value) value)))
(deftype lower-string ()
  '(satisfies lower-string-p))

(deftype simple-component-name () '(or lower-string symbol))
(deftype complex-component-name () '(or string symbol))
(deftype system-designator () '(or simple-component-name complex-component-name))

(deftype pathname-specifier ()
  '(or pathname string symbol))

(defun person-or-persons-p (value)
  (or (stringp value)
      (and (consp value)
           (every #'stringp value))))

(deftype person-or-persons ()
  '(satisfies person-or-persons-p))

(defun source-control-p (value)
  (and (consp value)
       (= (length value) 2)
       (keywordp (first value))
       (stringp (second value))))
(deftype source-control ()
  '(satisfies source-control-p))

(defun dependency (name &optional system-directory)
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

;;
;; Utilities

(defun ensure-list (value)
  (if (listp value)
      value
      (list value)))

(defmacro with-ignore-invalid ((label &optional (error-value nil)) &body body)
  `(handler-case (progn ,@body)
     (type-error (e)
       (warn "~A: Unexpected data found at ~S. Ignored.~%~A"
             (type-of e)
             ,label
             e)
       ,error-value)))

;;
;; Export functions

(defun name (value)
  (check-type value system-designator)
  (typecase value
    (string value)
    (otherwise (string-downcase value))))

(defun long-name (value)
  (with-ignore-invalid (:long-name)
    (check-type value (or string null))
    value))

(defun version (value &optional system-directory)
  (with-ignore-invalid (:version)
    (etypecase value
      (null value)
      (string value)
      (symbol (string-downcase value))
      (cons
        (destructuring-bind (type pathname &rest form-values)
            value
          (check-type pathname pathname-specifier)
          (ecase type
            (:read-file-form
             (apply #'uiop:read-file-form
                    (merge-pathnames pathname system-directory)
                    form-values))
            (:read-file-line
             (apply #'uiop:read-file-line
                    (merge-pathnames pathname system-directory)
                    form-values))))))))

(defun description (value)
  (with-ignore-invalid (:description)
    (check-type value (or string null))
    value))

(defun long-description (value)
  (with-ignore-invalid (:long-description)
    (check-type value (or string null))
    value))

(defun authors (value)
  (with-ignore-invalid (:author #())
    (check-type value (or person-or-persons null))
    (ensure-list value)))

(defun maintainers (value)
  (with-ignore-invalid (:maintainer #())
    (check-type value (or person-or-persons null))
    (ensure-list value)))

(defun mailto (value)
  (with-ignore-invalid (:mailto)
    (check-type value (or string null))
    value))

(defun license (value)
  (with-ignore-invalid (:license)
    (check-type value (or string symbol null))
    (etypecase value
      (null value)
      (string value)
      (symbol (symbol-value value)))))

(defun homepage (value)
  (with-ignore-invalid (:homepage)
    (check-type value (or string null))
    value))

(defun bug-tracker (value)
  (with-ignore-invalid (:bug-tracker)
    (check-type value (or string null))
    value))

(defun source-control (value)
  (with-ignore-invalid (:source-control)
    (check-type value (or source-control string null))
    (etypecase value
      (null value)
      (string (list nil value))
      (cons (list (string-downcase (first value))
                  (second value))))))

(defun depends-on (value &optional system-dir)
  (check-type value list)
  (if (null value)
      #()
      (mapcar (lambda (v) (dependency v system-dir)) value)))
