(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))

(defun len-i (lst)
  (let ((res 0))
    (dolist (x lst)
      (setf res (+ res 1)))
    res))

;;;   
;;; queue
;;;
      
(defun queue-new ()
  (cons nil nil))

(defun enqueue (q elt)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list elt)))
      (setf (cdr (cdr q)) (list elt)
	    (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

;;;
;;; Euclidean gcd
;;;

(defun e-gcd (a b)
  (if (zerop b)
      a
      (e-gcd b (mod a b))))
