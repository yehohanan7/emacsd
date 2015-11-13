
(defun print-elements (xs)
  (while xs
    (print (car xs))
    (setq xs (cdr xs))))

(defun row (xs) 
  (if (> (length xs) 1)
      (cons (+ (car xs) (cadr xs)) (row (cdr xs)))))

(defun gen-row (previous-row)
  (append '(1) (row previous-row) '(1)))

(defun pascal (n)
  (print '(1))
  (let ((row '(1 1)))
    (dotimes (nnumber n)
      (print row)
      (setq row (gen-row row)))))

(pascal 5)


