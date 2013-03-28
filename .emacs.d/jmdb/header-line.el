(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

  (defun sl/make-header ()
    ""
    (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
           (sl/header (file-name-directory sl/full-header))
           (sl/drop-str "[...]"))
      (if (> (length sl/full-header)
             (window-body-width))
          (if (> (length sl/header)
                 (window-body-width))
              (progn
                (concat (with-face sl/drop-str
                                   :background "blue"
                                   :weight 'normal
                                   )
                        (with-face (substring sl/header
                                              (+ (- (length sl/header)
                                                    (window-body-width))
                                                 (length sl/drop-str))
                                              (length sl/header))
                                   ;; :background "red"
                                   :weight 'normal
                                   )))
            (concat (with-face sl/header
                               ;; :background "red"
                               :foreground "#8fb28f"
                               :weight 'normal
                               )))
        (concat (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'normal
                           ;;:foreground "#8fb28f"
                           )
                (with-face (file-name-nondirectory buffer-file-name)
                           :weight 'normal
                           ;; :background "red"
                           )))))

  (defun sl/display-header ()
    (if (buffer-file-name)
        (setq header-line-format
              '("" ;; invocation-name
                (:eval (sl/make-header))))))


;;(sl/display-header)
(add-hook 'buffer-list-update-hook 'sl/display-header)


