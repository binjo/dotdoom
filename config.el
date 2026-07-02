;;; config.el -*- lexical-binding: t; -*-

(load! "+bindings")

(setq
 fancy-splash-image (expand-file-name "black-hole.png" doom-private-dir)
 doom-line-numbers-style 'relative
 display-line-numbers-type 'relative
 doom-localleader-key ","
 +default-repeat-forward-key ";"
 +default-repeat-backward-key "'"
 doom-large-file-size 10
 ;; doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size 16)
 doom-font (font-spec :family "CaskaydiaCove NFM" :size 22)
 doom-theme 'doom-tokyo-night
 ;; doom-unicode-font (font-spec :family "WenQuanYi Zen Hei Mono" :size 14)
 doom-unicode-extra-fonts nil)

;; Fix doom upgrade breaking on undefined variable
(setq comp-native-version-dir "~")

(setq +file-templates-dir
      (expand-file-name "templates/" (file-name-directory doom-private-dir)))

(setq native-compile-target-directory user-emacs-directory
      comp-native-version-dir user-emacs-directory)

(custom-set-faces!
  '(bookmark-face :background nil :foreground nil))

(when (not (display-graphic-p))
  (standard-display-unicode-special-glyphs))

(when IS-WINDOWS
  (setq
   doom-font (font-spec :family "CaskaydiaCove NFM" :size 13.0)
   doom-symbol-font (font-spec :family "Microsoft YaHei" :size 13.0))
  (copy-face 'default 'fixed-pitch))

(when IS-MAC
  (setq
   doom-font (font-spec :family "Source Code Pro" :size 14)
   doom-unicode-font (font-spec :family "WenQuanYi Zen Hei Mono" :size 14))
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (add-to-list 'default-frame-alist '(undecorated . t)) ;; for emacs-29?
  (setq display-line-numbers-type 'relative
        doom-line-numbers-style 'relative
        display-line-numbers 'relative)
  ;; maximize first frame
  (set-frame-parameter nil 'fullscreen 'maximized))

;; set before other settings
(when IS-MAC
  (if (file-directory-p "/Do_Not_Scan")
      (setq org-directory (file-truename (expand-file-name "org" "/Do_Not_Scan")))
    (setq org-directory
          (expand-file-name "org" doom-private-dir))))

(when IS-WINDOWS
  (if (file-directory-p "D:\\Exclusive\\repos")
      (setq org-directory (file-truename (expand-file-name "org" "D:\\Exclusive\\repos")))
    (setq org-directory
          (expand-file-name "org" doom-private-dir))))

(after! org
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  ;; state change log into drawer "LOGBOOK"
  (setq org-log-into-drawer t)

  (add-hook! 'org-mode-hook
             #'turn-on-font-lock
                #'toggle-truncate-lines
                #'visual-line-mode
                #'+org-pretty-mode))

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

  (add-to-list 'org-modules 'org-protocol)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "LATER(l)" "|" "DONE(d!)" "CANCELLED(c!)"))))

(after! org
  (add-to-list 'org-agenda-text-search-extra-files 'agenda-archives)

  (if (file-directory-p (expand-file-name "daily" org-roam-directory))
      (add-to-list 'org-agenda-files (expand-file-name "daily" org-roam-directory)))

  (setq org-outline-path-complete-in-steps nil
        org-fast-tag-selection-single-key t)

  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-custom-commands
        '(("W" "Completed and/or deferred tasks from previous week"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-day "-7d")
                        (org-agenda-entry-types '(:timestamp))
                        (org-agenda-show-log t)))))))

  (map! :leader
        :map evil-normal-state-map
        :desc "Weekly Review" "1" #'(lambda ()
                                      (interactive)
                                      (org-agenda nil "W"))))

(after! org
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
           :empty-lines 1))))

(use-package! org-super-agenda
  :after org-agenda
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
                                     (org-agenda nil "c"))))

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
         "a" #'org-roam-alias-add)))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table nil))

(use-package! vertico-posframe
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-center
        vertico-posframe-width 100
        vertico-posframe-min-width 100
        vertico-posframe-height 15
        vertico-posframe-min-height 15
        vertico-posframe-border-width 2
        vertico-posframe-fallback-mode #'ignore)

  ;; Use a modern horizontal ellipsis for truncation
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))

  ;; Common parameters for a clean look
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          (internal-border-width . 10)))

  ;; Linux-specific: Avoid parent-frame errors
  (when IS-LINUX
    (setq vertico-posframe-parameters
          (append '((parent-frame . nil)) vertico-posframe-parameters)))

  ;; Clean up any experimental advices/handlers from previous turns
  (advice-remove 'posframe--create-posframe #'+posframe--create-posframe-around-a)
  (advice-remove 'set-frame-parameter #'+set-frame-parameter-around-a)
  (setq posframe-arghandler nil)

  ;; Enable it via multiform for better control
  (after! vertico
    (vertico-multiform-mode 1)
    ;; Configure display modes for different commands
    (setq vertico-multiform-commands
          (if IS-MAC
              '((t posframe))
            '((consult-line posframe)))))

  (vertico-posframe-mode 1))

(map! :leader
      (:when (modulep! :completion vertico)
       :desc "M-x" "SPC" #'execute-extended-command))

(after! consult
  (setq consult-preview-key nil))

(use-package! affe
  :config
  ;; Tell Affe to use the Orderless compiler for fuzzy matching
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

  ;; (Optional) Customize the search command. 
  ;; By default, Affe will use 'find' and 'grep', but 'fd' and 'ripgrep' are much faster.
  (setq affe-find-command "fd --color=never --type f --hidden --exclude .git")
  
  (map! :leader
        :desc "Asynchronous fuzzy find" "f F" #'affe-find
        :desc "Asynchronous grep"       "s g" #'affe-grep))

(after! lsp-mode
  (if (file-directory-p "/Do_Not_Scan/tmp")
    (add-to-list 'lsp-file-watch-ignored-directories "/Do_Not_Scan/tmp")))

(advice-add #'company-ispell :around #'doom-shut-up-a)

(use-package! eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq eldoc-box-max-pixel-width 600
        eldoc-box-max-pixel-height 400))

(cond
  (IS-WINDOWS (setq my-yara-repo "d:/Exclusive/repos/yara-mode"))
  ((or IS-MAC IS-LINUX) (setq my-yara-repo "~/repos/yara-mode")))

(use-package! yara-mode
  :defer t
  :load-path my-yara-repo
  :mode ("\\.yara" "\\.yar")
  :config
  (add-hook! 'yara-mode-hook
             #'yas-minor-mode-on
             (defun +yara-disable-show-paren-mode-h ()
               (show-paren-mode -1)))
  (cond
   ((modulep! :tools lsp +lsp)
    (with-eval-after-load 'lsp-mode
      (add-hook! 'yara-mode-hook #'lsp)
      (add-to-list 'lsp-language-id-configuration
                   '(yara-mode . "yara"))
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection '("yr-ls"))
                        :activation-fn (lsp-activate-on-modes 'yara-mode)
                        :server-id 'yara-x-ls))))
   ((modulep! :tools lsp +eglot)
    (add-hook! 'yara-mode-hook #'eglot-ensure)
    (set-eglot-client! 'yara-mode '("yr-ls")))))

(use-package! powershell-ts-mode
  :defer t
  :config
  (setq powershell-ts-enable-imenu-top-level-vars nil))

(after! markdown-mode
  (setq markdown-fontify-code-blocks-natively t))

;; Fix for transient error on some Emacs versions
(unless (boundp 'overriding-text-conversion-style)
  (defvar overriding-text-conversion-style nil))

(after! magit
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
           (get-buffer-window transient--buffer t)))))

(after! transient
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
  (setq meow-cursor-type-beacon 'box)
  (setq blink-cursor-interval 0.3)
  (setq meow-use-clipboard t)
  (setq meow--delete-region-function 'kill-region)

  ;; Improve visibility for Tokyo Night theme
  (custom-set-faces!
    `(meow-face-selection :background ,(doom-color 'magenta) :foreground ,(doom-color 'base0))
    `(meow-face-search :background ,(doom-color 'green) :foreground ,(doom-color 'base0))
    `(meow-beacon-indicator :background ,(doom-color 'red) :foreground ,(doom-color 'base0) :weight bold)
    `(meow-beacon :background ,(doom-color 'red) :foreground ,(doom-color 'base0) :weight bold))

  ;; (map! :leader
  ;;       :desc "Kill current buffer" "k" #'kill-current-buffer)
  (meow-leader-define-key
   '(";" . "M-:")
   '("b" . "C-c w b")
   '("," . "C-c w b")
   '("c k" . kill-current-buffer)
   '("c r" . consult-recent-file)
   '("c x" . kill-current-buffer)
   '("f r" . consult-recent-file)
   '("p f" . projectile-find-file))
  (meow-normal-define-key
   '("." . meow-inner-of-thing)
   '("," . meow-bounds-of-thing)
   '("g" . dispatch-goto-menu))
  (add-to-list 'meow-expand-exclude-mode-list 'org-mode))

(after! evil-snipe
  (setq evil-snipe-scope 'buffer))

(use-package! valign
  :ensure t
  :delight valign-mode
  :custom (valign-fancy-bar t)
  :commands (valign-mode)
  :hook (org-mode . valign-mode))
