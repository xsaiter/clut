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


(defun area-right-triangle (a b c)
  (destructuring-bind (x y z) (sort (list a b c) #'<)
    (if (= (expt z 2) (+ (expt x 2) (expt y 2)))
	(/ (* x y) 2)
	nil)))


(defun n! (x)
  (if (= x 1)
      1
      (* x (n! (1- x)))))


(defun area-rect (h w)
  (* h w))


(defun area-circle (r)
  (* pi r r))



(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))



(defmacro x-while (predicate &body body)
  `(do ()
    ((not ,predicate))
     ,@body))


(defun test-x-while ()
  (let ((x 0))
    (x-while (< x 5)
	     (format t "text")
	     (incf x))))


(defmacro x-for (i beg end &body body)
  `(do ((,i ,beg (1+ ,i)))
	((> ,i  ,end) 'ok)
	,@body))


(defun test-for ()
    (x-for j 0 5
      (format t "iter")))

(defun my-open ()
  (let ((in (open "~/ttt/ls-books2.txt")))
    (format t "~a~%" (read-line in))
    (close in)))


