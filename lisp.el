'(:description "Buy Milk" :location "7-Eleven" :due-by "02-15-2013")

(setq todo1 '(:description "Buy Milk" :location "7-Eleven" :due-by
                           "02-15-2013"))
(getf todo1 :due-by)

(setq todo2 '(:description "Take out garbage" :location "Home" :due-by
                           "02-14-2013"))

(setq todos (list todo1 todo2))

(defun get-description (prop-list)
  (getf prop-list :description))

(mapcar 'get-description todos)
