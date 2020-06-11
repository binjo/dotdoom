;;; private/binjo/config.el -*- lexical-binding: t; -*-

;;;
(load! "+bindings")
;;
;; Plugins
;;

(use-package! yara-mode
  :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.yara" . yara-mode))
    (add-hook! 'yara-mode-hook #'(doom-enable-delete-trailing-whitespace-h yas-minor-mode-on))))


;;
;; Config
;;

(setq
 doom-line-numbers-style nil
 doom-localleader-key ","
 +default-repeat-forward-key ";"
 +default-repeat-backward-key "'"
 doom-large-file-size 10
 doom-font (font-spec :family "Source Code Pro" :size 14))

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
     'org-tempo-tags))
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
  (add-hook! 'org-mode-hook
    #'(turn-on-font-lock toggle-truncate-lines doom-enable-delete-trailing-whitespace-h))
  (setq org-outline-path-complete-in-steps nil
        org-fast-tag-selection-single-key t)
  (setq ;; org-agenda-custom-commands
        ;; '(("w" tags-todo "work")
        ;;   ("d" tags "adobe")
        ;;   ("r" tags "reading"))
        org-agenda-restore-windows-after-quit t)
  ;; (setq org-agenda-custom-commands
  ;;     '(("W" "Completed and/or deferred tasks from previous week"
  ;;      ((agenda "" ((org-agenda-span 7)
  ;;           (org-agenda-start-day "-7d")
  ;;           (org-agenda-entry-types '(:timestamp))
  ;;           (org-agenda-show-log t)))))))

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

  ;; ;; http://cestdiego.github.io/blog/2015/08/19/org-protocol/
  ;; (defadvice org-capture
  ;;     (after make-full-window-frame activate)
  ;;   "Advise capture to be the only window when used as a popup"
  ;;   (if (equal "org-capture" (frame-parameter nil 'name))
  ;;       (delete-other-frames)))

  ;; (defadvice org-capture-finalize
  ;;     (after delete-capture-frame activate)
  ;;   "Advise capture-finalize to close the frame"
  ;;   (when (equal "org-capture" (frame-parameter nil 'name))
  ;;       (delete-frame nil t)))

  (setq org-capture-templates
        '(("n" "ToDo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %?\n  :TIMESTAMP: %T\n"
           :empty-lines 1)
          ("t" "ToDo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %?\n  :TIMESTAMP: %T\n"
           :empty-lines 1)
          ;; create todo from org-protocol
          ("o" "ToDo" entry
           (file+headline "todo.org" "Todo")
           "* TODO %:description%?\n  :TIMESTAMP: %T\n\n%:link\n%:initial"
           :empty-lines 1)
          ("l" "Bookmarks" entry
           (file+headline "remember.org" "Bookmarks")
           "* %:description%?\n  :TIMESTAMP: %T\n\n%:link\n%:initial"
           :empty-lines 1)))

  (setq my-org-templates-dir
        (expand-file-name "org" +file-templates-dir))
  (defun my-new-daily-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "/tmp/reviews.org")
                                  ;; (file @,(expand-file-name "dailyreview.org" my-org-templates-dir))))))
                                  (file "~/.doom.d/templates/org/dailyreview.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (org-clock-in))))
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
  (setq org-agenda-custom-commands
        '(("c" "Super view"
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
                            (:discard (:not (:todo "TODO")))))))))))
  :config
  (org-super-agenda-mode)
  (map! :leader
        :map evil-normal-state-map
        :desc "Super Agenda" "2" #'(lambda ()
                                   (interactive)
                                   (org-agenda nil "c"))))

(after! org-oram
  (setq org-roam-directory (expand-file-name "roam" org-directory)))

(after! ivy
  (setq ivy-count-format "(%d/%d)")
  (setq ivy-use-virtual-buffers t))

(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-center))

(use-package! osx-dictionary
  :defer t
  :config
  (map! :leader
        (:desc "dictionary" :prefix "d"
        :desc "Search word at point and display result with buffer" :nv "w" #'osx-dictionary-search-pointer)))

;; (add-hook! 'python-mode-hook #'(doom|enable-delete-trailing-whitespace))
;; (add-hook 'python-mode-hook 'delete-trailing-whitespace)

;; (add-hook! :append :local (emacs-lisp-mode-hook python-mode-hook)
;;   #'(doom|enable-delete-trailing-whitespace))

;; (add-hook! :append 'doom-post-init-hook
;;   #'(global-display-line-numbers-mode -1))

;; (global-display-line-numbers-mode -1)

(provide 'config)
;;; config.el ends here
