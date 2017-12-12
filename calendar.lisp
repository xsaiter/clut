(defun leap-year-p (year)
  (or (zerop (mod year 400))
      (and (zerop (mod year 4))
	   (not (zerop (mod year 100))))))

(defun days-of-month (m y)
  (cond
    ((member m '(jan mar may jul aug oct dec)) 31)
    ((member m '(apr jun sep nov)) 30)
    ((member m '(feb)) (if (leap-year-p y) 29 28))))

