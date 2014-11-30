(add-hook 'find-file-hooks
          '(lambda ()
             (if (string-match "system" buffer-file-name)
                 (progn
                   (setq buffer-read-only t)
                   (message "Read only!!!")))))


(global-set-key (kbd "C-x 8") (lambda () (interactive) (insert "test")))


(request "curl http://www.iheartquotes.com/api/v1/random"
         :success (lambda (key, data)
                    (insert data)))


    (insert fortune)))


