(defun select-word ()
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)
    ))

;;(global-set-key (kbd "C-i") 'select-word)


(provide 'select-word)




