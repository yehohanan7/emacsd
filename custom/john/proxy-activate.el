(defun proxy-activate ()
  (interactive)
  (setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "10.33.99.65:80")
     ("https" . "10.33.99.65:80")))

  (setq url-http-proxy-basic-auth-storage
    (list (list "10.33.99.65:80"
                (cons "Input your LDAP UID !"
                      (base64-encode-string "tuijpx:Erlang$12345"))))))

(provide 'proxy-activate)


