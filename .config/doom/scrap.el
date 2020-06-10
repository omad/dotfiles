;;; ~/.config/doom/scrap.el -*- lexical-binding: t; -*-


;;; Not used
(defun dra/browse-url (url &optional new-window)
  "Call with a URL to open a url in the remote browser.

NEW-WINDOW is ignored"
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (concat "url=" (url-hexify-string url))))

    (url-retrieve "http://localhost:9999/openurl" 'my-kill-url-buffer)))

(defun my-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(when (string-match-p "compute.internal$" (system-name))
  (setq browse-url-browser-function 'dra/browse-url))

(defun my-url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
          (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    ;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
    (url-retrieve url 'my-switch-to-url-buffer)))


(defun my-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))
