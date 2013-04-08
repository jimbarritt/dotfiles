(defun current-hostname ()
  (first (split-string (shell-command-to-string "hostname") "\n")))

(defun current-user ()
  (first (split-string (shell-command-to-string "whoami") "\n")))


(defun rename-shell-to-host ()
  (interactive)
  (let ((buffer-name (concat "*shell:" (current-user) "@" (current-hostname) "*"))) 
    (message (concat "Renaming current buffer to " buffer-name))
    (rename-buffer buffer-name)))

(defun rename-shell (name)
  "…"
  (interactive "sEnter name for this shell: ")
  (let ((buffer-name (concat "*shell:" (current-hostname) ":" name "*"))) 
    (message (concat "Renaming current buffer to " buffer-name))
    (rename-buffer buffer-name)))

; turn off shell command echo
(defun my-comint-init () 
  (setq comint-process-echoes t)) 

;;(shell "/ssh:root@5.79.7.4:")



(defun ssh-shell (input)
  "…"
  (interactive "sSSH To: ")
  (let ((remote-dir (concat "/ssh:" input))) 
    (message (concat "Opening shell at " remote-dir))
    (cd remote-dir)
    (shell (concat "*shell:" (current-user) "@" (current-hostname) "*"))))
