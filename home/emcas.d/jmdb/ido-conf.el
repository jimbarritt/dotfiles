;; Interactive do
(when (not (package-installed-p 'ido-ubiquitous))
  (package-install 'ido-ubiquitous))


(require 'ido)
;;(require 'idomenu)
(require 'ido-ubiquitous)
(ido-mode t)

(icomplete-mode 1)

;;ido-save-directory-list-file "~/Dropbox/emacs/ido.last"

(setq
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-use-filename-at-point 'guess
 ido-max-prospects 12
 ido-auto-merge-work-directories-length -1
 ido-ignore-extensions t ; uses completion-ignored-extensions
 ido-use-virtual-buffers t
 ido-max-directory-size nil
 ido-max-work-file-list 50)


;; This should prevent the "Read-only enabled for this buffer but it doesnt!
(setq suggest-key-bindings nil ; Don't send me useless messages during M-x
      enable-recursive-minibuffers t)


;; (add-many-to-list 'completion-ignored-extensions
;;                '(".fdb_latexmk" ".prv" ".synctex.gz"))

(setq ido-ignore-buffers
      '("^ " "\.deft" "^\\*temp-shell*")
      ido-ignore-directories
      '("\\`\\.\\./" "\\`\\./")
      ido-ignore-files
      '("\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`__pycache__/")
      ido-file-extensions-order '(".el" ".hs" ".py"))

(setq minibuffer-prompt-properties
      '(read-only t
        point-entered minibuffer-avoid-prompt
        face minibuffer-prompt))

;; For virtual buffers
(setq recentf-max-saved-items 50)

;; Put ido completions on their own lines.
;; (setq ido-decorations
;;       '("\n → "
;;         ""
;;         "\n    "
;;         "\n    ..."
;;         "[" "]"
;;         " [No match]"
;;         " [Matched]"
;;         " [Not readable]"
;;         " [Too big]"
;;         " [Confirm]"))

;; (defun setup-ido () 
;;   (define-many-keys ido-completion-map
;;     `(("C- m" ido-merge-work-directories)
;;       ("C- <return>" ido-select-text)
;;       ("C- f" nil)
;;       ("C- x f" nil)
;;       ("C- x d" nil)
;;       ("C- b" nil)))

;;   (define-many-keys ido-file-dir-completion-map
;;     '(("+" ido-make-directory))))

;; (add-hook 'ido-minibuffer-setup-hook 'setup-ido)

(ido-mode 1)
(ido-everywhere 1) ; read-file-name etc
(ido-ubiquitous-mode 1) ; completing-read
