;; Jim's attempt to create a single header line at the top of emacs, displaying time.

;; See http://stackoverflow.com/questions/8842357/how-can-i-make-an-arbitrary-emacs-buffer-hidden
;; By putting a space at the beginning of a buffer name, it will be marked as "uninteresting" to emacs

(defconst *buffername* " *headerline*")

(defun create-header ()
  (interactive)
  (split-window-vertically 1)
  (get-buffer-create *buffername*)
  (switch-to-buffer *buffername*)
  (indsert "This is my new header"))

(create-header)

(get-buffer-create " *headerline*")


(kill-buffer " *headerline*")
