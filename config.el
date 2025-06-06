;;; private/binjo/config.el -*- lexical-binding: t; -*-

;;;
(load! "+bindings")
;;
;; Plugins
;;

(cond
  (IS-WINDOWS (setq my-yara-repo "d:/Exclusive/repos/yara-mode"))
  (IS-MAC (setq my-yara-repo "~/repos/yara-mode")))

(use-package! yara-mode
  :defer t
  :load-path my-yara-repo
  :mode "\\.yara"
  :config
  (add-hook! 'yara-mode-hook
             '(doom-enable-delete-trailing-whitespace-h
               yas-minor-mode-on))
  (cond
   ((modulep! :tools lsp +lsp)
    (with-eval-after-load 'lsp-mode
      (add-hook! 'yara-mode-hook #'lsp)
      (add-to-list 'lsp-language-id-configuration
                   '(yara-mode . "yara"))
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection "yls")
                        :activation-fn (lsp-activate-on "yara")
                        :server-id 'yls))))
   ((modulep! :tools lsp +eglot)
    (add-hook! 'yara-mode-hook #'eglot-ensure)
    (set-eglot-client! 'yara-mode '("yls")))))

;;
;; Config
;;
(setq
 fancy-splash-image "~/.doom.d/black-hole.png"
 doom-line-numbers-style nil
 doom-localleader-key ","
 +default-repeat-forward-key ";"
 +default-repeat-backward-key "'"
 doom-large-file-size 10
 display-line-numbers-type 'relative
 doom-line-numbers-style 'relative
 display-line-numbers 'relative
 doom-unicode-extra-fonts nil)

;; maximize first frame
;; (add-hook! 'doom-after-init-hook
;;   (set-frame-parameter nil 'fullscreen 'maximized)
;; )

(when IS-WINDOWS
  (setq
   doom-font (font-spec :family "CaskaydiaCove NFM" :size 13.0)
   doom-symbol-font (font-spec :family "Microsoft YaHei" :size 13.0)
   )
  ;; (set-face-attribute 'default nil
  ;;                     :family "CaskaydiaCove NFM"
  ;;                     :height 130
  ;;                     :weight 'normal
  ;;                     :width 'normal)
  (copy-face 'default 'fixed-pitch))

;; Fix doom upgrade breaking on undefined variable
(setq comp-native-version-dir "~")

(when IS-MAC
  (setq
   doom-font (font-spec :family "Source Code Pro" :size 14)
   doom-unicode-font (font-spec :family "WenQuanYi Zen Hei Mono" :size 14))
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (add-to-list 'default-frame-alist '(undecorated . t)) ;; for emacs-29?
  )

(setq +file-templates-dir
      (expand-file-name "templates/" (file-name-directory doom-private-dir)))

;; set before other settings
(when IS-MAC
  (if (file-directory-p "/Do_Not_Scan")
      (setq org-directory (file-truename (expand-file-name "org" "/Do_Not_Scan")))
    (setq org-directory
          (expand-file-name "org" doom-private-dir)))
  )
(when IS-WINDOWS
  (if (file-directory-p "D:\\Exclusive\\repos")
      (setq org-directory (file-truename (expand-file-name "org" "D:\\Exclusive\\repos")))
    (setq org-directory
          (expand-file-name "org" doom-private-dir)))
  )

(after! org
  (when (version<= "9.2" (org-version))
    (require 'org-tempo)
    (tempo-define-template
     "call-my-templates"
     '("#+CALL: " (P "call name: " callee 'noinsert) (s callee) "(\"" p "\")")
     "<k"
     "add call"
     'org-tempo-tags)
    (add-to-list 'org-tempo-keywords-alist '("0" . "roam_alias")))
  (add-to-list 'org-agenda-text-search-extra-files 'agenda-archives)
  ;; (when (featurep! :feature org-protocol)
  ;;   (add-to-list 'org-modules 'org-protocol))
  (add-to-list 'org-modules 'org-protocol)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "LATER(l)" "|" "DONE(d!)" "CANCELLED(c!)")))
  ;; (defvar binjo-org-files
  ;;   '("todo.org" "remember.org" "archive.org" "mapp.org" "gcal.org"))
  ;; (dolist (f binjo-org-files)
  ;;   (when (file-exists-p (expand-file-name f org-directory))
  ;;     (add-to-list 'org-agenda-files (expand-file-name f org-directory))))
  ;; (if (file-directory-p org-roam-directory)
  ;;     (add-to-list 'org-agenda-files org-roam-directory))
  (if (file-directory-p (expand-file-name "daily" org-roam-directory))
      (add-to-list 'org-agenda-files (expand-file-name "daily" org-roam-directory)))
  (add-hook! 'org-mode-hook
             #'turn-on-font-lock
                #'toggle-truncate-lines
                #'doom-enable-delete-trailing-whitespace-h
                #'visual-line-mode
                #'+org-pretty-mode)
  (setq org-outline-path-complete-in-steps nil
        org-fast-tag-selection-single-key t)
  (setq ;; org-agenda-custom-commands
   ;; '(("w" tags-todo "work")
   ;;   ("d" tags "adobe")
   ;;   ("r" tags "reading"))
   org-agenda-restore-windows-after-quit t)
  (setq org-agenda-custom-commands
        '(("W" "Completed and/or deferred tasks from previous week"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-day "-7d")
                        (org-agenda-entry-types '(:timestamp))
                        (org-agenda-show-log t)))))))

  (setq +org-capture-frame-parameters
        `((name . "org-capture")
          (width . 100)
          (height . 20)
          (transient . t)
          (top . 300)
          (left . 550)
          (window-system . ,(cond (IS-MAC 'ns)
                                  (IS-LINUX 'x)
                                  (t 'w32)))
          ,(if IS-LINUX '(display . ":0"))))

  (setq org-id-link-to-org-use-id 'create-if-interactive)

  ;; state change log into drawer "LOGBOOK"
  (setq org-log-into-drawer t)

  ;; http://cestdiego.github.io/blog/2015/08/19/org-protocol/
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "org-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  ;; (defadvice org-capture-finalize
  ;;     (after delete-capture-frame activate)
  ;;   "Advise capture-finalize to close the frame"
  ;;   (when (equal "org-capture" (frame-parameter nil 'name))
  ;;       (delete-frame nil t)))
  ;; https://emacs-china.org/t/emacs-mac-org-capture-emacs/13453
  (defun osx-switch-back-to-previous-application ()
    "Switch back to previous application on macOS."
    (interactive)
    (do-applescript
     (mapconcat
      #'identity
      '("tell application \"System Events\""
        "  tell process \"Finder\""
        "    activate"
        "    keystroke tab using {command down}"
        "  end tell"
        "end tell")
      "\n")))

  (defun org-capture-finalize@after (&rest r)
    (when (equal "o" (plist-get org-capture-plist :key))
      (run-at-time 0 nil #'osx-switch-back-to-previous-application)))
  (advice-add #'org-capture-finalize :after  #'org-capture-finalize@after)

  (setq org-capture-templates
        '(("n" "ToDo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %?\n:PROPERTIES:\n:ID:       %(org-id-uuid)\n:TIMESTAMP: %T\n:END:\n"
           :empty-lines 1)
          ("t" "ToDo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %?\n:PROPERTIES:\n:ID:       %(org-id-uuid)\n:TIMESTAMP: %T\n:END:\n"
           :empty-lines 1)
          ;; create todo from org-protocol
          ("o" "ToDo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %:description%?\n:PROPERTIES:\n:ID:       %(org-id-uuid)\n:TIMESTAMP: %T\n:END:\n\n%:link\n%:initial"
           :empty-lines 1)
          ("l" "Bookmarks" entry
           (file+headline "remember.org" "Bookmarks")
           "* %:description%?\n:PROPERTIES:\n:ID:       %(org-id-uuid)\n:TIMESTAMP: %T\n:END:\n\n%:link\n%:initial"
           :empty-lines 1)))

  ;; (setq my-org-templates-dir
  ;;       (expand-file-name "org" +file-templates-dir))
  ;; (defun my-new-daily-review ()
  ;;   (interactive)
  ;;   (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "/tmp/reviews.org")
  ;;                                   ;; (file @,(expand-file-name "dailyreview.org" my-org-templates-dir))))))
  ;;                                   (file "~/.doom.d/templates/org/dailyreview.org")))))
  ;;     (progn
  ;;       (org-capture nil "d")
  ;;       (org-capture-finalize t)
  ;;       (org-speed-move-safe 'outline-up-heading)
  ;;       (org-narrow-to-subtree)
  ;;       (org-clock-in))))
  (map! :leader
        :map evil-normal-state-map
        :desc "Weekly Review" "1" #'(lambda ()
                                      (interactive)
                                      (org-agenda nil "W"))
        )
  )

(use-package! org-super-agenda
  :after org-agenda
  ;; :defer t
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  (add-to-list 'org-agenda-custom-commands
               '("c" "Super view"
                 ((agenda "" ((org-agenda-overriding-header "")
                              (org-super-agenda-groups
                               '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)))))
                  (alltodo "" ((org-agenda-overriding-header "")
                               (org-super-agenda-groups
                                '((:log t)
                                  ;; (:name "To refile"
                                  ;;        :file-path "refile\\.org")
                                  (:name "Important"
                                   :priority "A"
                                   :order 1)
                                  (:name "Work"
                                   :tag ("work" "jira")
                                   :order 2)
                                  (:name "Due Today"
                                   :deadline today
                                   :order 2)
                                  (:name "Scheduled"
                                   :scheduled future
                                   :order e)
                                  (:name "Later Todo"
                                   :todo "LATER"
                                   :order 3)
                                  (:name "Overdue"
                                   :deadline past
                                   :order 7)
                                  (:name "Life"
                                   :tag ("fin")
                                   :order 10)
                                  (:discard (:not (:todo "TODO"))))))))))
  :config
  (org-super-agenda-mode)
  (map! :leader
        :map evil-normal-state-map
        :desc "Super Agenda" "2" #'(lambda ()
                                     (interactive)
                                     (org-agenda nil "c"))
        ))

(after! org-roam
  (setq org-roam-directory (file-truename (expand-file-name "roam" org-directory)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* TODO %?\n:PROPERTIES:\n:ID:       %(org-id-uuid)\n:TIMESTAMP: %T\n:END:\n"
           :if-new
           (file+head+olp "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d>\n\n* Clear TODOs\n\n" ("Clear TODOs"))
           :unnarrowed t
           :jump-to-captured t
           :empty-lines 1)))
  (map! :map org-mode-map
        :localleader
        (:prefix ("r" . "refile")
         "f" #'binjo/refile-to-roam-file
         "m" #'binjo/refile-to-roam-file-2
         "F" #'binjo/refile-to-roam-today-file
         "C" #'binjo/refile-from-headline)
        (:prefix ("m" . "org-roam")
         "a" #'org-roam-alias-add))
  ;; (add-hook! 'org-capture-after-finalize-hook #'org-roam-db-autosync--try-update-on-save-h)
  )

;; (after! ivy
;;   (setq ivy-count-format "(%d/%d)")
;;   (setq ivy-use-virtual-buffers t))

;; (after! ivy-posframe
;;   (setf (alist-get t ivy-posframe-display-functions-alist)
;;         #'ivy-posframe-display-at-frame-center))

;; (use-package! osx-dictionary
;;   :defer t
;;   :config
;;   (map! :leader
;;         (:desc "dictionary" :prefix "d"
;;          :desc "Search word at point and display result with buffer" :nv "w" #'osx-dictionary-search-pointer)))

;; (add-hook! 'python-mode-hook #'(doom|enable-delete-trailing-whitespace))
;; (add-hook 'python-mode-hook 'delete-trailing-whitespace)

;; (add-hook! :append :local (emacs-lisp-mode-hook python-mode-hook)
;;   #'(doom|enable-delete-trailing-whitespace))

;; (add-hook! :append 'doom-post-init-hook
;;   #'(global-display-line-numbers-mode -1))

;; (global-display-line-numbers-mode -1)

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table nil))

(use-package! rime
  :init
  (setq rime-librime-root "~/.local/librime/dist")
  (setq rime-user-data-dir (file-truename "~/Library/Rime"))
  :after-call after-find-file pre-command-hook
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  :config
  (defadvice! +rime--posframe-display-result-a (args)
    "给 `rime--posframe-display-result' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
    :filter-args #'rime--posframe-display-result
    (cl-destructuring-bind (result) args
      (let ((newresult (if (string-blank-p result)
                           result
                         (concat result "　"))))
        (list newresult))))

  (load! "+rime-probe-english"))

(map! :leader
      ;; (:when (featurep! :completion ivy)
      ;; :desc "M-x" "SPC" 'counsel-M-x)
      (:when (modulep! :completion vertico)
       :desc "M-x" "SPC" #'execute-extended-command)
       )

(after! evil-snipe
  (setq evil-snipe-scope 'buffer))

(custom-set-faces!
  '(bookmark-face :background nil :foreground nil))

(advice-add #'company-ispell :around #'doom-shut-up-a)

(use-package! valign
  :ensure t
  :delight valign-mode
  :custom (valign-fancy-bar t)
  :commands (valign-mode)
  :hook (org-mode . valign-mode))

(after! consult
  (setq consult-preview-key nil))

(use-package! vertico-posframe
  :config
  (vertico-posframe-mode 1))

(after! lsp-mode
  (if (file-directory-p "/Do_Not_Scan/tmp")
    (add-to-list 'lsp-file-watch-ignored-directories "/Do_Not_Scan/tmp")))

(after! markdown-mode
  (setq markdown-fontify-code-blocks-natively t))

(after! magit
  ;; :config
  ;; By default a thin line whose color indicates the transient-ness of the
  ;; menu is used.  Without an echo area that would look odd and below we
  ;; color the border instead.
  (setq transient-mode-line-format nil)

  (setq transient-display-buffer-action
        (list
         (lambda (buffer _)
           (posframe-show
            buffer
            :poshandler #'posframe-poshandler-frame-center
            ;; To reduce the likelyhood of horizontal resizing, use the
            ;; same minimal width as transient uses by default.  It matches
            ;; the width needed to display the commands common to all menus.
            :min-width transient-minimal-frame-width
            ;; If the parent frame is small, there might not be enough room.
            ;; By default posframe wraps lines, but we truncate instead.
            :lines-truncate t
            ;; Enable the fringe, so that we can see when truncation has
            ;; occured.  Hm, actually that's not good enough, so let's not.
            ;; :right-fringe 8
            ;;
            ;; Indicate transient-ness of the menu.  You could also use a
            ;; constant color, if you don't care about this.
            :internal-border-color (transient--prefix-color)
            :internal-border-width 1)
           ;; `posframe-show' it not suitable for use as a display action
           ;; and it appears posframe does not provide some other function
           ;; that is.  We can make this more complient by at least
           ;; returning the used window.
           (get-buffer-window transient--buffer t))))
  (transient-define-prefix dispatch-goto-menu () "This isn't documentation"
    [["Move"
      ("b" "bottom" end-of-buffer)
      ("g" "top" beginning-of-buffer)
      ("d" "definition (xref)" xref-find-definitions)
      ("h" "beginning of line" beginning-of-line)
      ("e" "end of line" end-of-line)
      ("s" "first non-blank-line" beginning-of-line-text)]
     ["Buffer"
      ("n" "next buffer" next-buffer)
      ("p" "previous buffer" previous-buffer)
      ("B" "bury buffer" bury-buffer)
      ("U" "unbury buffer" unbury-buffer)
      "Avy"
      ("c" "goto char" avy-goto-char)
      ("l" "got line" avy-goto-line)]
     ]))

(after! meow
  (setq meow-cursor-type-normal 'box)
  (setq blink-cursor-interval 0.3)
  (setq meow-use-clipboard t)
  (meow-leader-define-key
   '(";" . "M-:")
   '("b" . "C-c w b")
   '("," . "C-c w b")
   '("c k" . kill-current-buffer)
   '("c r" . consult-recent-file)
   )
  (meow-normal-define-key
   '("." . meow-inner-of-thing)
   '("," . meow-bounds-of-thing)
   '("g" . dispatch-goto-menu))
  (add-to-list 'meow-expand-exclude-mode-list 'org-mode))

(use-package! powershell-ts-mode
  :defer t
  :config
  (setq powershell-ts-enable-imenu-top-level-vars nil))

(provide 'config)
;;; config.el ends here
