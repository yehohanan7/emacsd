(defun fortune ()
  (interactive)
  (let ((buffer (url-retrieve-synchronously "http://www.iheartquotes.com/api/v1/random")))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq fortune (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    (insert fortune)))

(provide 'fortune)










