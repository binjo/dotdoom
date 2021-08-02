;;; private/binjo/config.el -*- lexical-binding: t; -*-

;;;
(load! "+bindings")
;;
;; Plugins
;;

(use-package! yara-mode
  :after yasnippet
  :load-path "~/repos/yara-mode"
  :mode "\\.yara"
  :config
  (add-hook! 'yara-mode-hook #'(doom-enable-delete-trailing-whitespace-h yas-minor-mode-on)))

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
 doom-font (font-spec :family "Source Code Pro" :size 14)
 doom-unicode-font (font-spec :family "WenQuanYi Zen Hei Mono" :size 14)
 doom-unicode-extra-fonts nil)

(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq display-line-numbers-type nil)
  ;; maximize first frame
  (set-frame-parameter nil 'fullscreen 'maximized))

(setq +file-templates-dir
      (expand-file-name "templates/" (file-name-directory doom-private-dir)))

;; set before other settings
(if (file-directory-p "/Do_Not_Scan")
    (setq org-directory (expand-file-name "org" "/Do_Not_Scan"))
  (setq org-directory
        (expand-file-name "org" doom-private-dir)))

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
  (if (file-directory-p org-roam-directory)
      (add-to-list 'org-agenda-files org-roam-directory))
  (if (file-directory-p (expand-file-name "daily" org-roam-directory))
      (add-to-list 'org-agenda-files (expand-file-name "daily" org-roam-directory)))
  (add-hook! 'org-mode-hook
             #'(turn-on-font-lock
                toggle-truncate-lines
                doom-enable-delete-trailing-whitespace-h
                visual-line-mode))
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
  (setq org-roam-directory (expand-file-name "roam" org-directory))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?" :if-new
           (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d>\n\n* Clear TODOs\n\n")
           :unnarrowed t
           :empty-lines 1)))
  (map! :map org-mode-map
        :localleader
        (:prefix ("r" . "refile")
         "f" #'binjo/refile-to-roam-file
         "m" #'binjo/refile-to-roam-file-2
         "F" #'binjo/refile-to-roam-today-file
         "C" #'binjo/refile-from-headline)
        (:prefix ("m" . "org-roam")
         "a" #'org-roam-alias-add)))

(after! ivy
  (setq ivy-count-format "(%d/%d)")
  (setq ivy-use-virtual-buffers t))

(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-center))

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

(set-email-account! "binjo"
  '((mu4e-sent-folder       . "/sent")
    (mu4e-drafts-folder     . "/drafts")
    (mu4e-trash-folder      . "/trash")
    ;; (mu4e-refile-folder     . "/personal/All Mail")
    (smtpmail-smtp-user     . "binjo.cn@gmail.com")
    (user-mail-address      . "binjo.cn@gmail.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nbinjo"))
  t)

(use-package! mu4e-thread-folding
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))
  (map! :map mu4e-headers-mode-map
        ("<tab>" #'mu4e-headers-toggle-at-point
         "<left>" #'mu4e-headers-fold-at-point
         "<S-left>" #'mu4e-headers-fold-all
         "<right>" #'mu4e-headers-unfold-at-point
         "<S-right>" #'mu4e-headers-unfold-all)))

(map! :leader
      (:when (featurep! :completion ivy)
      :desc "M-x" "SPC" 'counsel-M-x)
      ;; (:when (featurep! :completion vertico)
      ;;  :desc "M-x" "SPC" '))
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

(provide 'config)
;;; config.el ends here
