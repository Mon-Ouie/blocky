;;; Brute force debugging
    
(defun blocky-trace-all ()
  (do-external-symbols (sym (find-package :blocky))
    (when (fboundp sym)
      (ignore-errors (eval `(trace ,sym))))))

(defun blocky-untrace-all ()
  (do-external-symbols (sym (find-package :blocky))
    (when (fboundp sym)
      (ignore-errors (eval `(untrace ,sym))))))

;;; Printing a block

(defun print-block (B)
  (let (fields)
    (flet ((add-field (field value)
	     (push (list field value) fields)))
      (typecase B
	(blocky:object 
	 (let ((f2 (object-fields B)))
	   (etypecase f2
	     (hash-table (maphash #'add-field f2))
	     (list (setf fields f2)))
	   (cons (get-some-object-name B) fields)))
	(list (mapcar #'print-block B))
	(otherwise B)))))

