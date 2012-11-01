;; General Window features:

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window-Options.html#Choosing-Window-Options

(add-to-list 'load-path "~/.emacs.d/lib/maxframe")

(require 'maxframe)
(setq mf-max-width 1600)  ;; Pixel width of main monitor.
;;(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'ns-toggle-fullscreen t)

;; Hide  scroll bars and toolbar:
;; Emacs gurus don't need no stinking scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Make the minibuffer smaller
(setq max-mini-window-height 1)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)



;; POPWIN - Controls size of popup windows
;; https://github.com/m2ym/popwin-el
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 0.2)

(push '("^\\*magit.*\\*$"  :regexp) popwin:special-display-config)
(push "*Backtrace*" popwin:special-display-config)
;; M-x dired-jump-other-window
(push '(dired-mode :position top) popwin:special-display-config)

;; M-!
(push "*Shell Command Output*" popwin:special-display-config)

;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

;; slime
(push "*slime-apropos*" popwin:special-display-config)
(push "*slime-macroexpansion*" popwin:special-display-config)
(push "*slime-description*" popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push "*slime-xref*" popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push 'slime-repl-mode popwin:special-display-config)
(push 'slime-connection-list-mode popwin:special-display-config)

;; vc
(push "*vc-diff*" popwin:special-display-config)
(push "*vc-change-log*" popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)

(push "*Buffer List*" popwin:special-display-config)
