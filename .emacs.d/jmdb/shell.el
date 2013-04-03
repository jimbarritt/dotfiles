(defun current-hostname ()
  (first (split-string (shell-command-to-string "hostname") "\n")))

(defun rename-shell-to-host ()
  (interactive)
  (let ((buffer-name (concat "*shell:" (current-hostname) "*"))) 
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

(add-hook 'comint-mode-hook 'my-comint-init) 


