(defun insert-test ()
  (interactive)
  (insert "test \" \" do \n \n end"))

(global-set-key (kbd "C-x t") 'insert-test)

(provide 'elixir-insert-test)
