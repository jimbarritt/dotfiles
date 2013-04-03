(defun current-hostname ()
  (first (split-string (shell-command-to-string "hostname") "\n")))

(defun rename-shell-buffer ()
  (interactive)
  (let ((buffer-name (concat "*shell:" (current-hostname) "*"))) 
    (message (concat "Renaming current buffer to " buffer-name))
    (rename-buffer buffer-name)))




