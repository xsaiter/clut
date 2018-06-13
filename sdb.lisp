(defpackage :xsaiter.sdb (:use :common-lisp))

(defvar *db* nil)


(defun make-person (name age)
  (list :name name :age age))


(defun add-person-to-db (person)
  (push person  *db*))


(defun clear-db ()
  (setf *db* nil))


(defun display-db ()
  (print *db*))


(defun save-db-to-file (filename)
  (with-open-file (fp filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* fp))))


(defun load-db-from-file (filename)
  (with-open-file (fp filename)
    (with-standard-io-syntax
      (setf *db* (read fp)))))


(defun select (predicate)
  (remove-if-not predicate *db*))


(defun where (&key name age)
  #'(lambda (x)
      (and
       (if name (equal (getf x :name) name) t)
       (if age (equal (getf x :age) age) t))))


