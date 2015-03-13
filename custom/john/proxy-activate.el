(defun proxy-activate ()
  (interactive)

  (let ((proxy "host:port") (credentials "username:password"))
    (setq url-proxy-services
      `(("no_proxy" . "^\\(localhost\\|10.*\\)")
       ("http" . ,proxy)
       ("https" . ,proxy)))

    (setq url-http-proxy-basic-auth-storage
      (list (list proxy
                (cons "Input your LDAP UID !"
                      (base64-encode-string credentials)))))))

(provide 'proxy-activate)


