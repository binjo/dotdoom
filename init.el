;;; private/binjo/init.el -*- lexical-binding: t; -*-
;; 2018.07.18 @ FEYE

;; * Modules
(doom! :feature
       ;; debugger
       eval
       (evil
        +everywhere)
       file-templates
       ;; snippets
       ;; spellcheck
       (syntax-checker +childframe)
       workspaces
       lookup

       :completion
       (company +auto +childframe)
       ;; (ivy +childframe)
       ivy

       :ui
       ;; vc-gutter
       doom
       doom-dashboard
       ;; (doom-modeline +new)
       doom-modeline
       hl-todo
       ;; fci
       ;; nav-flash
       ;; neotree
       ;; treemacs
       ;; pretty-code
       (popup
        +all
        +defaults)
       window-select
       :editor
       rotate-text
       :emacs
       vc
       dired
       ;; electric
       eshell
       term
       ediff
       ;; imenu

       :tools
       ;; editorconfig
       ;; ein
       gist
       macos
       ;; make
       magit
       ;; rgb
       ;; reference
       ;; upload
       ;; tmux
       ;; password-store

       :lang
       ;; lsp
       data
       (python +lpy +conda)
       ;; ess
       ;; (latex
       ;;  +latexmk
       ;;  +skim)
       (org
        +attach
        +babel
        +capture
        +present)
       emacs-lisp
       javascript
       markdown
       ;; sh
       (web +html)
       go

       ;; :app
       ;; sx
       ;; rss
       ;; ;; twitter
       ;; email
       ;; (write
       ;;  +wordnut
       ;;  +osxdict
       ;;  +synosaurus
       ;;  +langtool)

       :config
       (default +snippets +evil-commands +bindings))
